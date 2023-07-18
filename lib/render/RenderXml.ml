open Prelude
open Bwd
open Core

module E = RenderEff.Perform

type printer = Xmlm.output -> unit

type part =
  | Top
  | Frontmatter
  | Mainmatter
  | Backmatter

type cfg = {part : part; trail : int bwd; counter : int ref}

module Printer =
struct
  module P0 =
  struct
    type out = Xmlm.output

    let text txt out =
      Xmlm.output out @@ `Data txt
  end

  include PrinterKit.Kit (P0)
end

module Xml =
struct
  let tag name attrs bdy : printer =
    let attrs' = attrs |> List.map @@ fun (k,v) -> ("", k), v in
    fun out ->
      Xmlm.output out @@ `El_start (("", name), attrs');
      Printer.seq ~sep:Printer.space bdy out;
      Xmlm.output out `El_end

  let with_xsl stylesheet bdy : printer =
    fun out ->
    let line = Format.sprintf "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" stylesheet in
    Xmlm.output out @@ `Dtd (Some line);
    bdy out
end



let rec render_node ~cfg : Sem.node -> printer =
  function
  | Sem.Text txt ->
    Printer.text txt
  | Sem.Math (mode, bdy) ->
    let attrs = 
      match mode with
      | Inline -> [] 
      | Display -> ["display", "block"]
    in
    let module TP = RenderMathMode.Printer in
    Xml.tag "tex" attrs [
      Printer.text @@
      TP.contents @@
      RenderMathMode.render bdy
    ]
  | Sem.Link {title; dest} ->
    begin
      match E.get_doc dest with
      | Some _ ->
        render_internal_link ~cfg ~title ~addr:dest
      | None ->
        render_external_link ~cfg ~title ~url:dest
    end
  | Sem.Tag (name, xs) ->
    Xml.tag name [] [render ~cfg xs]
  | Sem.Transclude {mode; addr; toc} ->
    begin
      match E.get_doc addr, mode with
      | None, _ ->
        failwith @@ Format.sprintf "Failed to transclude non-existent tree with address '%s'" addr
      | Some doc, mode ->
        render_transclusion ~cfg ~mode ~toc doc
    end
  | Sem.Query (title, mode, query) ->
    let docs = E.run_query query in
    begin
      match docs with 
      | [] -> Printer.nil 
      | _ ->
        let body = docs |> List.filter_map @@ fun (doc : Sem.doc) -> 
          doc.addr |> Option.map @@ fun addr ->
          Sem.Transclude {mode = Collapsed; toc = false; addr}
        in
        let doc : Sem.doc = 
          {addr = None;
           taxon = None;
           title = Some title;
           authors = [];
           date = None;
           metas = [];
           tags = [];
           body = body}
        in
        render_transclusion ~cfg ~mode ~toc:true doc
    end
  | Sem.EmbedTeX {packages; source} ->
    let code =
      RenderMathMode.Printer.contents @@
      RenderMathMode.render source
    in
    let hash = Digest.to_hex @@ Digest.string code in
    E.enqueue_latex ~name:hash ~packages ~source:code;
    let path = Format.sprintf "resources/%s-web.svg" hash in
    Xml.tag "center" []
      [Xml.tag "img" ["src", path] []]
  | Sem.Block (title, body) ->
    Xml.tag "block" ["open", "open"] @@
    [Xml.tag "headline" [] [render ~cfg title];
     render ~cfg body]

and render_transclusion ~cfg ~mode ~toc doc = 
  let cfg = 
    match mode with
    | Spliced -> cfg 
    | _ -> 
      let ctr = cfg.counter in
      let ix = !ctr + 1 in
      ctr := ix;
      {cfg with trail = Snoc(cfg.trail, ix); counter = ref 0} 
  in
  render_doc ~mode ~cfg ~toc doc


and render_internal_link ~cfg ~title ~addr =
  let url = E.route addr in
  Xml.tag "link"
    ["href", url; "type", "local"]
    [render ~cfg title]

and render_external_link ~cfg ~title ~url =
  Xml.tag "link"
    ["href", url; "type", "external"]
    [render ~cfg title]

and render ~cfg : Sem.t -> printer =
  Printer.iter (render_node ~cfg)

and render_author (author : string) =
  let cfg = {part = Frontmatter; trail = Emp; counter = ref 0} in
  (* If the author string is an address to a biographical page, then link to it *)
  match E.get_doc author with
  | Some bio ->
    begin 
      match bio.addr with 
      | None -> 
        Printer.text author
      | Some addr -> 
        let url = E.route addr in
        Xml.tag "link"
          ["href", url; "type", "local"]
          [match bio.title with
           | None -> Printer.text "Untitled"
           | Some title -> render ~cfg title]
    end
  | None ->
    Printer.text author

and render_date (doc : Sem.doc) =
  match doc.date with
  | None -> Printer.nil
  | Some date ->
    let str = Format.asprintf "%a" Date.pp_human date in
    Xml.tag "date" [] [Printer.text str]

and render_authors (doc : Sem.doc) =
  let contributors = 
    match doc.addr with 
    | Some addr -> E.contributors addr
    | None -> [] 
  in 
  match doc.authors, contributors with
  | [], [] -> Printer.nil
  | authors, contributors ->
    Xml.tag "authors" [] [
      begin
        authors |> Printer.iter @@ fun author ->
        Xml.tag "author" [] [render_author author]
      end;
      begin
        contributors |> Printer.iter @@ fun contributor ->
        Xml.tag "contributor" [] [render_author contributor]
      end
    ]

and with_addr (doc : Sem.doc) k = 
  match doc.addr with 
  | Some addr -> k addr 
  | None -> Printer.nil 

and render_frontmatter ~cfg ?(toc = true) (doc : Sem.doc) =
  let anchor = string_of_int @@ Oo.id (object end) in
  Xml.tag "frontmatter" [] [
    if toc then render_trail cfg.trail else Printer.nil;
    Xml.tag "anchor" [] [Printer.text anchor];
    with_addr doc (fun addr -> Xml.tag "addr" [] [Printer.text addr]);
    with_addr doc begin fun addr -> 
      match E.abs_path addr with
      | Some abspath ->
        Xml.tag "abspath" [] [Printer.text abspath]
      | None ->
        Printer.nil
    end;
    with_addr doc (fun addr -> Xml.tag "route" [] [Printer.text @@ E.route addr]);
    render_date doc;
    render_authors doc;
    begin
      doc.title |> Printer.option @@ fun title ->
      Xml.tag "title" [] [
        render ~cfg:{cfg with part = Frontmatter} @@
        Sem.sentence_case title
      ]
    end;
    begin
      doc.metas |> Printer.iter @@ fun (key, body) ->
      Xml.tag "meta" ["name", key] [render ~cfg:{cfg with part = Frontmatter} body]
    end
  ]

and render_mainmatter ~cfg (doc : Sem.doc) =
  Xml.tag "mainmatter" [] [
    render ~cfg:{cfg with part = Mainmatter} doc.body
  ]

and render_backmatter ~cfg (doc : Sem.doc) =
  let cfg = {cfg with part = Backmatter} in
  with_addr doc @@ fun addr ->
  Xml.tag "backmatter" [] [
    Xml.tag "contributions" [] [
      E.contributions addr |> Printer.iter @@
      render_doc ~cfg
    ];
    Xml.tag "context" [] [
      E.parents addr |> Printer.iter @@
      render_doc ~cfg
    ];
    Xml.tag "related" [] [
      E.related addr |> Printer.iter @@
      render_doc ~cfg
    ];
    Xml.tag "backlinks" [] [
      E.backlinks addr |> Printer.iter @@
      render_doc ~cfg
    ];
    Xml.tag "references" [] [
      E.bibliography addr |> Printer.iter @@
      render_doc ~cfg
    ];
  ]

and render_trail trail =
  let render_crumb i = Xml.tag "crumb" [] [Printer.text @@ string_of_int i] in
  Xml.tag "trail" [] @@
  List.map render_crumb @@ 
  Bwd.to_list trail

and mode_to_string =
  function
  | Full -> "full"
  | Spliced -> "spliced"
  | Collapsed -> "collapsed"

and bool_to_string =
  function
  | true -> "true"
  | false -> "false"


and trail_to_string =
  function 
  | Emp -> ""
  | Snoc (trail, i) -> 
    Format.sprintf "%s.%i" (trail_to_string trail) i

and render_doc ~cfg ?(mode = Full) ?(toc = true) (doc : Sem.doc) : printer =
  let module S = Algaeff.Sequencer.Make (struct type elt = string * string end) in
  let attrs = 
    List.of_seq @@ S.run @@ fun () -> 
    S.yield ("mode", mode_to_string mode);
    doc.addr |> Option.iter (fun addr -> S.yield ("root", bool_to_string @@ E.is_root addr));
    doc.taxon |> Option.iter (fun taxon -> S.yield ("taxon", StringUtil.sentence_case taxon))
  in
  Xml.tag "tree" attrs
    [render_frontmatter ~cfg ~toc doc;
     render_mainmatter ~cfg doc;
     match cfg.part with
     | Top -> render_backmatter ~cfg doc
     | _ -> Printer.nil]

let render_doc_page ~trail (doc : Sem.doc) : printer =
  Xml.with_xsl "forest.xsl" @@
  render_doc ~cfg:{trail; part = Top; counter = ref 0} doc
