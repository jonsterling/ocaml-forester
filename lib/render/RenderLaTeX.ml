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

let render_date =
  Format.dprintf {|\date{%a}@.|} Date.pp_human

let rec render  (nodes :  Sem.t) : Printer.t = 
  Printer.iter render_node  nodes

and render_node : Sem.node -> Printer.t = 
  function 
  | Text txt -> Printer.text txt
  | Transclude (_, _, addr) -> 
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
        Format.dprintf {|\ForesterRef{%s}{%a}|} dest (Fun.flip render) title
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
    let path = Format.sprintf "resources/%s.pdf" hash in
    Format.dprintf {|\begin{center}\includegraphics{%s}\end{center}%s|} path "\n"
  | Block (title, body) -> 
    Printer.seq [
      Format.dprintf {|\begin{proof}[{%a}]%s|} (Fun.flip render) title "\n";
      render body;
      Format.dprintf {|\end{proof}%s|} "\n"
    ]

and render_title title = 
  Format.dprintf {|\title{%a}%s|} (Fun.flip render) title "\n"

and render_tag name body = 
  let name = 
    match name with
    | "p" -> "par"
    | "b" | "strong" -> "textbf"
    | "em" -> "emph"
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
  | [], _ -> Printer.nil 
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

and render_doc_section (doc : Sem.doc) : Printer.t =
  let title = Option.value ~default:[] doc.title in
  let taxon = Option.value ~default:"" doc.taxon in
  Printer.seq ~sep:(Printer.text "\n") [
    Printer.nil;
    Format.dprintf 
      {|\begin{tree}{title={%a}, taxon={%s}, slug={%s}}|}
      (Fun.flip render) title
      taxon
      doc.addr;
    render doc.body;
    Format.dprintf {|\end{tree}|};
    Printer.nil;
  ]

let render_doc_page (doc : Sem.doc) : Printer.t = 
  let contributors = E.contributors doc.addr in
  Printer.seq ~sep:(Printer.text "\n") [
    Format.dprintf {|\documentclass{article}|};
    Format.dprintf {|\usepackage{amsmath,mathtools,forester}|};
    doc.title |> Printer.option render_title;
    doc.date |> Printer.option render_date;
    render_authors (doc.authors, contributors);
    Format.dprintf {|\begin{document}|};
    Format.dprintf {|\maketitle|};
    render doc.body;
    Format.dprintf {|\end{document}|}
  ]
