open Bwd
open Prelude
open Core

module E = RenderEff.Perform

module Printer =
struct
  module P0 =
  struct
    type out = Format.formatter
    let text = Format.dprintf "%s"
  end

  include PrinterKit.Kit (P0)
end


let rec add_qedhere xs =
  match Bwd.of_list xs with 
  | Emp -> xs
  | Snoc (xs', last) ->
    let qedhere = Sem.Tag ("qedhere", []) in
    match last with 
    | Sem.Tag ("ol", ys) -> 
      Bwd.to_list @@ Bwd.Snoc (xs', Sem.Tag ("ol", add_qedhere ys))
    | Sem.Tag ("ul", ys) ->
      Bwd.to_list @@ Bwd.Snoc (xs', Sem.Tag ("ul", add_qedhere ys))
    | Sem.Tag ("li", ys) ->
      Bwd.to_list @@ Bwd.Snoc (xs', Sem.Tag ("li", add_qedhere ys))
    | Sem.Math (Display, ys) -> 
      Bwd.to_list @@ Bwd.Snoc (xs', Sem.Math (Display, add_qedhere ys))
    | _ -> 
      Bwd.to_list @@ Bwd.Snoc (Bwd.Snoc (xs', last), qedhere)


let render_date =
  Format.dprintf {|\date{%a}@.|} Date.pp_human

let rec render  (nodes : Sem.t) : Printer.t = 
  Printer.iter render_node  nodes

and render_node : Sem.node -> Printer.t = 
  function 
  | Text txt -> Printer.text txt
  | Transclude (_, addr) -> 
    begin 
      match E.get_doc addr with
      | None ->
        failwith @@ Format.sprintf "Failed to transclude non-existent tree with address '%s'" addr
      | Some doc -> 
        render_doc_section doc
    end
  | Tag (name, body) ->
    render_tag name body
  | Link {title; dest} -> 
    begin
      match E.get_doc dest with
      | None ->
        Format.dprintf {|\href{%s}{%a}|} dest (Fun.flip render) title
      | Some doc ->
        begin 
          match doc.taxon with 
          | Some "reference" ->
            Format.dprintf {|%a~\cite{%s}|} (Fun.flip render) title dest
          | Some "person" -> 
            render title
          | _ ->
            Format.dprintf {|\ForesterRef{%s}{%a}|} dest (Fun.flip render) title
        end
    end   
  | Math (Inline, body) ->
    Format.dprintf {|\(%a\)|} (Fun.flip RenderMathMode.render) body
  | Math (Display, body) -> 
    Format.dprintf {|\[%a\]|} (Fun.flip RenderMathMode.render) body
  | EmbedTeX {source; packages} -> 
    let code =
      RenderMathMode.Printer.contents @@
      RenderMathMode.render source
    in
    let hash = Digest.to_hex @@ Digest.string code in
    E.enqueue_latex ~name:hash ~packages ~source:code;
    let path = Format.sprintf "resources/%s-print.pdf" hash in
    Format.dprintf {|\[\includegraphics{%s}\]%s|} path "\n"
  | Block (title, body) -> 
    Printer.seq [
      Format.dprintf {|\begin{proof}[{%a}]%s|} (Fun.flip render) title "\n";
      render @@ add_qedhere body;
      Format.dprintf {|\end{proof}%s|} "\n"
    ]
  | Query _ ->
    Printer.nil

and render_title title = 
  Format.dprintf {|\title{%a}%s|} (Fun.flip render) (Sem.sentence_case title) "\n"

and render_tag name body =
  match name with 
  | "ol" ->
    Printer.seq ~sep:(Printer.text "\n") [
      Format.dprintf {|\begin{enumerate}|};
      render body;
      Format.dprintf {|\end{enumerate}|};
    ]
  | "ul" ->
    Printer.seq ~sep:(Printer.text "\n") [
      Format.dprintf {|\begin{itemize}|};
      render body;
      Format.dprintf {|\end{itemize}|};
    ]
  | "blockquote" ->
    Printer.seq ~sep:(Printer.text "\n") [
      Format.dprintf {|\begin{quotation}|};
      render body;
      Format.dprintf {|\end{quotation}|};
    ]
  | _ -> 
    let name = 
      match name with
      | "p" -> "par"
      | "b" | "strong" -> "textbf"
      | "em" -> "emph"
      | "li" -> "item"
      | _ -> name
    in 
    Format.dprintf {|\%s{%a}|} name (Fun.flip render) body


and render_author author = 
  match E.get_doc author with
  | Some bio ->
    begin 
      match bio.title with 
      | Some title -> render title
      | None -> Printer.text author
    end
  | None ->
    Printer.text author

and render_authors =
  function 
  | [], [] -> Printer.nil 
  | authors, contributors ->
    let pp_sep fmt () = Format.fprintf fmt {| \and |} in
    Format.dprintf {|\author{%a%a}%s|}
      (Format.pp_print_list ~pp_sep (Fun.flip render_author))
      authors
      (Fun.flip render_contributors) contributors
      "\n"

and render_contributors = 
  function 
  | [] -> Printer.nil 
  | contributors -> 
    let pp_sep fmt () = Format.fprintf fmt {|, |} in
    Format.dprintf {|\thanks{With contributions from %a.}|}
      (Format.pp_print_list ~pp_sep (Fun.flip render_author))
      contributors

and strip_first_paragraph xs = 
  match xs with 
  | Sem.Tag ("p", body) :: rest -> body @ rest
  | _ -> xs

and render_doc_section (doc : Sem.doc) : Printer.t =
  let title = Sem.sentence_case @@ Option.value ~default:[] doc.title in
  let taxon = Option.value ~default:"" doc.taxon in
  let addr = Option.value doc.addr ~default:(string_of_int @@ Oo.id (object end)) in
  Printer.seq ~sep:(Printer.text "\n") [
    Printer.nil;
    Format.dprintf 
      {|\begin{tree}{title={%a}, taxon={%s}, slug={%s}}|}
      (Fun.flip render) title
      taxon
      addr;
    render @@ strip_first_paragraph doc.body;
    Format.dprintf {|\end{tree}|};
    Printer.nil;
  ]

let render_base_url url = 
  Format.dprintf {|\ForesterSetup{forestSite = {%s}}|} url

let render_doc_page ~base_url (doc : Sem.doc) : Printer.t = 
  let contributors = 
    match doc.addr with 
    | Some addr -> E.contributors addr
    | None -> []
  in
  Printer.seq ~sep:(Printer.text "\n") [
    Format.dprintf {|\documentclass[a4paper]{article}|};
    Format.dprintf {|\usepackage[final]{microtype}|};
    Format.dprintf {|\usepackage{fontspec}|};
    Format.dprintf {|\setmonofont{inconsolata}|};
    Format.dprintf {|\usepackage{amsmath,amsthm,amssymb,stmaryrd,mathtools,biblatex,forester}|};
    Format.dprintf {|\addbibresource{forest.bib}|};
    base_url |> Printer.option render_base_url;
    doc.title |> Printer.option render_title;
    doc.date |> Printer.option render_date;
    render_authors (doc.authors, contributors);
    Format.dprintf {|\begin{document}|};
    Format.dprintf {|\maketitle|};
    render doc.body;
    Format.dprintf {|\printbibliography|};
    Format.dprintf {|\end{document}|}
  ]
