open Prelude
open Bwd
open Core

module E = Render_effect.Perform

module Printer = Xml_printer
type printer = Printer.printer

type cfg =
  {base_url : string option;
   in_backmatter : bool;
   top : bool;
   seen : addr list}

let rec render_node ~cfg : Sem.node Range.located -> printer =
  fun located ->
  match located.value with
  | Sem.Text txt ->
    Printer.text txt
  | Sem.Math (mode, bdy) ->
    let attrs =
      match mode with
      | Inline -> []
      | Display -> [Printer.attr "display" "block"]
    in
    let module TP = Render_verbatim.Printer in
    Printer.tag "tex" attrs [
      Printer.text @@
      TP.contents @@
      Render_verbatim.render ~cfg:{tex = false} bdy
    ]
  | Sem.Link {title; dest; modifier} ->
    begin
      match E.get_doc dest with
      | Some _ ->
        render_internal_link ~cfg ~title ~modifier ~addr:dest
      | None ->
        render_external_link ~cfg ~title ~modifier ~url:dest
    end
  | Sem.Ref {addr} ->
    begin
      match E.get_doc addr with
      | None ->
        Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%s` for reference" addr
      | Some tree ->
        let url = E.route Xml addr in
        let attrs =
          (Printer.attr "addr" addr) ::
          (Printer.attr "href" url) ::
          match tree.fm.taxon with
          | None -> []
          | Some taxon -> [Printer.attr "taxon" @@ String_util.sentence_case taxon]
        in
        Printer.tag "ref" attrs []
    end
  | Sem.Xml_tag (name, attrs, xs) ->
    let attrs =
      attrs |> List.map @@ fun (k, v) ->
      let txt =
        Render_verbatim.Printer.contents @@
        Render_verbatim.render ~cfg:{tex = true} v
      in
      Printer.attr k txt
    in
    Printer.tag name attrs [render ~cfg xs]
  | Sem.Unresolved name ->
    Reporter.fatalf ?loc:located.loc Resolution_error
      "unresolved identifier `\\%s`" name
  | Sem.Transclude (opts, addr) ->
    begin
      match E.get_doc addr with
      | None ->
        Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%s` for transclusion" addr
      | Some doc ->
        render_transclusion ~cfg ~opts doc
    end
  | Sem.Subtree (opts, subtree) ->
    render_transclusion ~cfg ~opts subtree
  | Sem.Query (opts, query) ->
    if not cfg.in_backmatter then
      let docs = E.run_query query in
      match docs with
      | [] -> Printer.nil
      | _ ->
        let body =
          docs |> List.filter_map @@ fun (doc : Sem.tree) ->
          doc.fm.addr |> Option.map @@ fun addr ->
          let opts = Sem.{expanded = false; show_heading = true; title_override = None; taxon_override = None; toc = false; numbered = false; show_metadata = true} in
          Range.locate_opt None @@ Sem.Transclude (opts, addr)
        in
        let doc : Sem.tree =
          {fm =
             {addr = None;
              taxon = None;
              title = None;
              authors = [];
              contributors = [];
              dates = [];
              metas = [];
              tags = [];
              source_path = None};
           body}
        in
        render_transclusion ~cfg ~opts doc
    else
      Printer.nil
  | Sem.Embed_tex {preamble; source} ->
    let as_tex x =
      Render_verbatim.Printer.contents @@
      Render_verbatim.render ~cfg:{tex = true} x
    in
    let preamble = as_tex preamble in
    let source = as_tex source in
    let hash = Digest.to_hex @@ Digest.string @@ preamble ^ source in
    E.enqueue_latex ~name:hash ~preamble ~source;
    Printer.tag "embedded-tex" [Printer.attr "hash" hash] [
      Printer.tag "embedded-tex-preamble" [] [Printer.text preamble];
      Printer.tag "embedded-tex-body" [] [Printer.text source]
    ]
  | Sem.Img {path} ->
    Printer.tag "img" [Printer.attr "src" path] []
  | Sem.Block (title, body) ->
    Printer.tag "block" [Printer.attr "open" "open"] @@
    [Printer.tag "headline" [] [render ~cfg title];
     render ~cfg body]
  | Sem.If_tex (_, x) ->
    render ~cfg x
  | Sem.Prim (p, x) ->
    let name =
      match p with
      | `P -> "p"
      | `Ul -> "ul"
      | `Ol -> "ol"
      | `Li -> "li"
      | `Em -> "em"
      | `Strong -> "strong"
      | `Code -> "code"
      | `Blockquote -> "blockquote"
      | `Pre -> "pre"
    in
    Printer.tag name [] [render ~cfg x]
  | Sem.Object _ ->
    Reporter.fatal ?loc:located.loc Type_error
      "tried to render object closure to XML"

and render_transclusion ~cfg ~opts doc =
  let cfg =
    {cfg with top = false}
  in
  render_tree ~cfg ~opts doc

and render_internal_link ~cfg ~title ~modifier ~addr =
  let url = E.route Xml addr in
  let doc = E.get_doc addr in
  let doc_title = Option.bind doc @@ fun d -> d.fm.title in
  let title = Option.fold title ~none:doc_title ~some:Option.some in
  let target_title_attr =
    match doc_title with
    | None -> []
    | Some t ->
      let title_string =
        String_util.sentence_case @@
        Render_text.Printer.contents @@
        Render_text.render t
      in
      [Printer.attr "title" title_string]
  in
  let title = Option.map (Sem.apply_modifier modifier) title in
  let title = Option.value ~default:[Range.locate_opt None @@ Sem.Text addr] title in
  Printer.tag "link"
    ([Printer.attr "href" url; Printer.attr "type" "local"; Printer.attr "addr" addr] @ target_title_attr)
    [render ~cfg title]

and render_external_link ~cfg ~title ~modifier ~url =
  let title = Option.map (Sem.apply_modifier modifier) title in
  let title = Option.value ~default:[Range.locate_opt None @@ Sem.Text url] title in
  Printer.tag "link"
    [Printer.attr "href" url; Printer.attr "type" "external"]
    [render ~cfg title]


and render ~cfg : Sem.t -> printer =
  Printer.iter (render_node ~cfg)

and render_author (author : string) =
  let cfg = {base_url = None; top = false; in_backmatter = false; seen = []} in
  (* If the author string is an address to a biographical page, then link to it *)
  match E.get_doc author with
  | Some bio ->
    begin
      match bio.fm.addr with
      | None ->
        Printer.text author
      | Some addr ->
        let url = E.route Xml addr in
        Printer.tag "link"
          [Printer.attr "href" url; Printer.attr "type" "local"; Printer.attr addr "addr"]
          [match bio.fm.title with
           | Some title ->
             render ~cfg title
           | _ -> Printer.text "Untitled"
          ]
    end
  | None ->
    Printer.text author

and render_date (doc : Sem.tree) =
  let inner_render_date date =
    let date_addr = Format.asprintf "%a" Date.pp date in
    let attrs =
      match E.get_doc date_addr with
      | None -> []
      | Some _ -> [Printer.attr "href" @@ E.route Xml date_addr]
    in
    Printer.tag "date" attrs [
      Printer.tag "year" [] [Printer.text @@ string_of_int @@ Date.year date];
      Date.month date |> Printer.option begin fun m ->
        Printer.tag "month" [] [Printer.text @@ string_of_int m]
      end;
      Date.day date |> Printer.option begin fun d ->
        Printer.tag "day" [] [Printer.text @@ string_of_int d]
      end;
    ]
  in
  Printer.iter inner_render_date doc.fm.dates

and render_authors (doc : Sem.tree) =
  let contributors =
    match doc.fm.addr with
    | Some addr -> E.contributors addr
    | None -> []
  in
  match doc.fm.authors, contributors with
  | [], [] -> Printer.nil
  | authors, contributors ->
    Printer.tag "authors" [] [
      begin
        authors |> Printer.iter @@ fun author ->
        Printer.tag "author" [] [render_author author]
      end;
      begin
        contributors |> Printer.iter @@ fun contributor ->
        Printer.tag "contributor" [] [render_author contributor]
      end
    ]

and with_addr (doc : Sem.tree) k =
  match doc.fm.addr with
  | Some addr -> k addr
  | None -> Printer.nil

and render_rss_link ~cfg doc =
  (* Only link to RSS if there is a base url, because RSS cannot be generated in the first place without one. *)
  cfg.base_url |> Printer.option @@ fun _ ->
  with_addr doc @@ fun addr ->
  Printer.tag "rss" [] [Printer.text (E.route Rss addr)]

and render_frontmatter ~cfg ?(toc = true) (doc : Sem.tree) =
  let anchor = string_of_int @@ Oo.id (object end) in
  Printer.tag "frontmatter" [] [
    Printer.tag "anchor" [] [Printer.text anchor];
    render_rss_link ~cfg doc;
    doc.fm.taxon |> Printer.option begin fun taxon ->
      Printer.tag "taxon" [] [Printer.text @@ String_util.sentence_case taxon]
    end;
    with_addr doc begin fun addr ->
      Printer.tag "addr" [] [Printer.text addr]
    end;
    begin
      doc.fm.source_path |> Printer.option @@ fun path ->
      Printer.tag "source-path" [] [Printer.text path]
    end;
    with_addr doc begin fun addr ->
      Printer.tag "route" [] [Printer.text @@ E.route Xml addr]
    end;
    render_date doc;
    render_authors doc;
    begin
      doc.fm.title |> Printer.option @@ fun title ->
      Printer.tag "title" [] [
        render ~cfg @@
        Sem.sentence_case title
      ]
    end;
    begin
      doc.fm.metas |> Printer.iter @@ fun (key, body) ->
      Printer.tag "meta" [Printer.attr "name" key] [render ~cfg body]
    end
  ]

and render_mainmatter ~cfg (doc : Sem.tree) =
  Printer.tag "mainmatter" [] [
    render ~cfg doc.body
  ]

and render_backmatter ~cfg (doc : Sem.tree) =
  let cfg = {cfg with in_backmatter = true; top = false} in
  let opts = Sem.{title_override = None; taxon_override = None; toc = false; show_heading = true; expanded = false; numbered = false; show_metadata = true} in

  with_addr doc @@ fun addr fmt ->
  Reporter.tracef "when rendering backmatter of `%s` to XML" addr @@ fun () ->
  Printer.tag "backmatter" [] [
    Printer.tag "contributions" [] [
      E.contributions addr |> Printer.iter @@
      render_tree ~cfg ~opts
    ];
    Printer.tag "context" [] [
      E.parents addr |> Printer.iter @@
      render_tree ~cfg ~opts
    ];
    Printer.tag "related" [] [
      E.related addr |> Printer.iter @@
      render_tree ~cfg ~opts
    ];
    Printer.tag "backlinks" [] [
      E.backlinks addr |> Printer.iter @@
      render_tree ~cfg ~opts
    ];
    Printer.tag "references" [] [
      E.bibliography addr |> Printer.iter @@
      render_tree ~cfg ~opts
    ];
  ] fmt

and bool_to_string =
  function
  | true -> "true"
  | false -> "false"

and render_tree ~cfg ~opts (doc : Sem.tree) : printer =
  let doc =
    match opts.title_override with
    | Some _ as title -> {doc with fm = {doc.fm with title}}
    | None -> doc
  in
  let doc =
    match opts.taxon_override with
    | Some _ as taxon -> {doc with fm = {doc.fm with taxon}}
    | None -> doc
  in
  let attrs =
    [Printer.attr "expanded" @@ string_of_bool opts.expanded;
     Printer.attr "show-heading" @@ string_of_bool opts.show_heading;
     Printer.attr "show-metadata" @@ string_of_bool opts.show_metadata;
     Printer.attr "toc" @@ string_of_bool opts.toc;
     Printer.attr "numbered" @@ string_of_bool opts.numbered;
     Printer.attr "root" @@ string_of_bool @@ Option.fold doc.fm.addr ~none:false ~some:(fun addr -> E.is_root addr)]
  in
  let seen =
    match doc.fm.addr with
    | None -> false
    | Some addr -> List.mem addr cfg.seen
  in
  let trace k =
    match doc.fm.addr with
    | None -> k ()
    | Some addr ->
      Reporter.tracef "when rendering tree at address `%s` to XML" addr k
  in
  if seen then
    Printer.nil
  else
    let cfg =
      match doc.fm.addr with
      | None -> cfg
      | Some addr ->
        {cfg with seen = addr :: cfg.seen}
    in
    fun fmt ->
      trace @@ fun () ->
      Printer.tag "tree" attrs
        [render_frontmatter ~cfg ~toc:opts.toc doc;
         render_mainmatter ~cfg doc;
         match cfg.top with
         | true -> render_backmatter ~cfg doc
         | _ -> Printer.nil
        ] fmt

let render_tree_page ~base_url (doc : Sem.tree) : printer =
  let cfg = {base_url; top = true; seen = []; in_backmatter = false} in
  let opts = Sem.{title_override = None; taxon_override = None; toc = false; show_heading = true; expanded = true; numbered = false; show_metadata = true} in
  Printer.with_xsl "forest.xsl" @@ render_tree ~cfg ~opts doc
