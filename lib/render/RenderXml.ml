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

type cfg = {part : part; trail : int bwd}

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
  | Sem.Transclude (ix, mode, addr) ->
    begin
      match E.get_doc addr, mode with
      | None, _ ->
        failwith @@ Format.sprintf "Failed to transclude non-existent tree with address '%s'" addr
      | Some doc, mode ->
        let cfg = {cfg with trail = Snoc(cfg.trail, ix)} in
        render_doc ~mode ~cfg doc
    end
  | Sem.EmbedTeX {packages; source} ->
    let code =
      RenderMathMode.Printer.contents @@
      RenderMathMode.render source
    in
    let hash = Digest.to_hex @@ Digest.string code in
    E.enqueue_latex ~name:hash ~packages ~source:code;
    let path = Format.sprintf "resources/%s.svg" hash in
    Xml.tag "center" []
      [Xml.tag "img" ["src", path] []]
  | Sem.Block (title, body) ->
    Xml.tag "block" ["open", "open"] @@
    [Xml.tag "headline" [] [render ~cfg title];
     render ~cfg body]

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
  let cfg = {part = Frontmatter; trail = Emp} in
  (* If the author string is an address to a biographical page, then link to it *)
  match E.get_doc author with
  | Some bio ->
    let url = E.route bio.addr in
    Xml.tag "link"
      ["href", url; "type", "local"]
      [match bio.title with
       | None -> Printer.text "Untitled"
       | Some title -> render ~cfg title]
  | None ->
    Printer.text author

and render_date (doc : Sem.doc) =
  match doc.date with
  | None -> Printer.nil
  | Some date ->
    let str = Format.asprintf "%a" Date.pp_human date in
    Xml.tag "date" [] [Printer.text str]

and render_authors (doc : Sem.doc) =
  match doc.authors, E.contributors doc.addr with
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

and render_frontmatter ~cfg (doc : Sem.doc) =
  Xml.tag "frontmatter" [] [
    render_trail cfg.trail;
    Xml.tag "addr" [] [Printer.text doc.addr];
    begin
      match E.abs_path doc.addr with
      | Some abspath ->
        Xml.tag "abspath" [] [Printer.text abspath]
      | None ->
        Printer.nil
    end;
    Xml.tag "route" [] [Printer.text @@ E.route doc.addr];
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
  Xml.tag "backmatter" [] [
    Xml.tag "contributions" [] [
      E.contributions doc.addr |> Printer.iter @@
      render_doc ~cfg
    ];
    Xml.tag "context" [] [
      E.parents doc.addr |> Printer.iter @@
      render_doc ~cfg
    ];
    Xml.tag "related" [] [
      E.related doc.addr |> Printer.iter @@
      render_doc ~cfg
    ];
    Xml.tag "backlinks" [] [
      E.backlinks doc.addr |> Printer.iter @@
      render_doc ~cfg
    ];
    Xml.tag "references" [] [
      E.bibliography doc.addr |> Printer.iter @@
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

and render_doc ~cfg ?(mode = Full) (doc : Sem.doc) : printer =
  let module S = Algaeff.Sequencer.Make (struct type elt = string * string end) in
  let attrs = 
    List.of_seq @@ S.run @@ fun () -> 
    S.yield ("mode", mode_to_string mode);
    S.yield ("root", bool_to_string @@ E.is_root doc.addr);
    doc.taxon |> Option.iter (fun taxon -> S.yield ("taxon", StringUtil.sentence_case taxon))
  in
  Xml.tag "tree" attrs
    [render_frontmatter ~cfg doc;
     render_mainmatter ~cfg doc;
     match cfg.part with
     | Top -> render_backmatter ~cfg doc
     | _ -> Printer.nil]

let render_doc_page ~trail (doc : Sem.doc) : printer =
  Xml.with_xsl "forest.xsl" @@
  render_doc ~cfg:{trail; part = Top} doc
