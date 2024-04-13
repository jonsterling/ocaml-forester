open Core
open Prelude

open Dream_html

module E = Render_effect.Perform
module F = Dream_forester

module String_map = Map.Make (String)

module Ancestors = Algaeff.Reader.Make (struct type t = addr list end)
module Current_addr = Algaeff.Reader.Make (struct type t = addr option end)
module Mainmatter_cache = Algaeff.State.Make (struct type t = node String_map.t end)

module Xmlns_map =
struct
  type t =
    {prefix_to_xmlns : string String_map.t;
     xmlns_to_prefixes : string list String_map.t}

  let empty =
    {prefix_to_xmlns = String_map.empty;
     xmlns_to_prefixes = String_map.empty}

  let assoc ~prefix ~xmlns env =
    {prefix_to_xmlns = String_map.add prefix xmlns env.prefix_to_xmlns;
     xmlns_to_prefixes = String_map.add_to_list xmlns prefix env.xmlns_to_prefixes}
end

module Xmlns_prefixes = Algaeff.Reader.Make (Xmlns_map)

let rec normalise_prefix ?loc ~prefix ~xmlns kont =
  match prefix, xmlns with
  | Some prefix, Some xmlns ->
    begin
      let open Xmlns_map in
      let env = Xmlns_prefixes.read () in
      let exception Shadowing in
      try
        begin
          match
            String_map.find_opt prefix env.prefix_to_xmlns,
            String_map.find_opt xmlns env.xmlns_to_prefixes
          with
          | None, (None | Some []) ->
            let env = assoc ~prefix ~xmlns env in
            Xmlns_prefixes.run ~env @@ fun () ->
            kont @@ ([(prefix, xmlns)], Some prefix)
          | Some xmlns', Some prefixes ->
            if xmlns' = xmlns && List.mem prefix prefixes then
              kont ([], Some prefix)
            else
              raise Shadowing
          | _, Some (prefix' :: _) ->
            kont ([], Some prefix')
          | Some xmlns', None ->
            raise Shadowing
        end
      with Shadowing ->
        normalise_prefix ?loc ~prefix:(Some (prefix ^ "_")) ~xmlns:(Some xmlns) kont
    end
  | _ ->
    kont ([], prefix)

let optional kont opt =
  match opt with
  | Some x -> kont x
  | None -> F.null []

let optional_ kont opt =
  match opt with
  | Some x -> kont x
  | None -> F.null_

let render_date (date : Date.t) =
  let date_addr = Format.asprintf "%a" Date.pp date in
  F.date [
    E.get_doc date_addr |> optional_ @@ fun _ ->
    F.href "%s" @@ E.route date_addr
  ] [
    F.year [] "%i" (Date.year date);
    Date.month date |> optional @@ F.month [] "%i";
    Date.day date |> optional @@ F.day [] "%i"
  ]

let render_dates (dates : Date.t list) =
  F.null @@ List.map render_date dates


let rec render_located (located : Sem.node Range.located) =
  match located.value with
  | Sem.Prim (p, x) ->
    F.prim p [] [render_nodes x]

  | Sem.Text text ->
    txt "%s" text

  | Sem.Math (mode, body) ->
    let rendered =
      let module TP = Render_verbatim.Printer in
      Str.global_replace (Str.regexp "\n") " " @@
      TP.contents @@ Render_verbatim.render ~cfg:{tex = false} body
    in
    F.tex [
      match mode with
      | Inline -> F.null_
      | Display -> F.display "block"
    ] "%s" rendered

  | Sem.Link {title; dest; modifier} ->
    begin
      match E.get_doc dest with
      | Some tree ->
        render_internal_link ~title ~modifier ~addr:dest ~dest:tree
      | None ->
        render_external_link ~title ~modifier ~url:dest
    end

  | Sem.Ref {addr} ->
    begin
      match E.get_doc addr with
      | None ->
        Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%s` for reference" addr
      | Some tree ->
        let url = E.route addr in
        F.ref [
          F.addr_ "%s" addr;
          F.href "%s" url;
          tree.fm.taxon |> Option.map String_util.sentence_case |> optional_ @@ F.taxon_ "%s";
          tree.fm.number |> optional_ @@ F.number_ "%s"
        ]
    end

  | Sem.Img {path} ->
    F.img [F.src "%s" path]

  | Sem.If_tex (_, x) ->
    render_nodes x

  | Sem.Xml_tag (Xml_resolved_qname name, attrs, xs) ->

    let rec fold_attrs tag_prefix updates acc attrs  =
      match attrs with
      | [] ->
        let xmlns_attrs =
          updates |> List.map @@ fun (prefix, xmlns) ->
          string_attr ("xmlns:" ^ prefix) "%s" xmlns
        in
        let tag_name =
          match tag_prefix with
          | Some prefix -> prefix ^ ":" ^ name.uname
          | None -> name.uname
        in
        std_tag
          tag_name
          (xmlns_attrs @ List.rev acc)
          [render_nodes xs]

      | (Xml_resolved_qname k, v) :: attrs ->
        normalise_prefix ?loc:located.loc ~prefix:k.prefix ~xmlns:k.xmlns @@ fun (updates', prefix) ->
        let xml_attr =
          let name =
            match prefix with
            | Some prefix -> prefix ^ ":" ^ k.uname
            | None -> k.uname
          in
          string_attr name "%s" @@
          Render_verbatim.Printer.contents @@
          Render_verbatim.render ~sep:Render_verbatim.Printer.nil ~cfg:{tex = false} v
        in
        fold_attrs tag_prefix (updates @ updates') (xml_attr :: acc) attrs
    in

    normalise_prefix ~prefix:name.prefix ~xmlns:name.xmlns @@ fun (updates, tag_prefix) ->
    fold_attrs tag_prefix updates [] attrs

  | Sem.Unresolved name ->
    Reporter.fatalf ?loc:located.loc Resolution_error
      "unresolved identifier `\\%s`" name

  | Sem.Object _ ->
    Reporter.fatal ?loc:located.loc Type_error
      "tried to render object closure to XML"

  | Sem.Embed_tex {preamble; source} ->
    let as_tex x =
      Render_verbatim.Printer.contents @@
      Render_verbatim.render ~cfg:{tex = true} x
    in
    let preamble = as_tex preamble in
    let source = as_tex source in
    let hash = Digest.to_hex @@ Digest.string @@ preamble ^ source in
    E.enqueue_latex ~name:hash ~preamble ~source;
    F.embedded_tex [F.hash "%s" hash] [
      F.embedded_tex_preamble [] "%s" preamble;
      F.embedded_tex_body [] "%s" source
    ]

  | Sem.Transclude (opts, addr) ->
    begin
      match E.get_doc addr with
      | None ->
        Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%s` for transclusion" addr
      | Some doc ->
        render_transclusion ~opts doc
    end

  | Sem.Subtree (opts, subtree) ->
    render_transclusion ~opts subtree

  | Sem.Query (opts, query) ->
    let trees = E.run_query query in
    match trees with
    | [] -> F.null []
    | _ ->
      let body =
        trees |> List.filter_map @@ fun (tree : Sem.tree) ->
        tree.fm.addr |> Option.map @@ fun addr ->
        let opts = Sem.{expanded = false; show_heading = true; title_override = None; taxon_override = None; toc = false; numbered = false; show_metadata = true} in
        Range.locate_opt None @@ Sem.Transclude (opts, addr)
      in
      render_transclusion ~opts {fm = Sem.empty_frontmatter; body}

and render_nodes nodes =
  F.null @@ List.map render_located nodes


and render_transclusion ~opts (tree : Sem.tree) =
  let current = Current_addr.read () in
  let update old_ancestors =
    match current with
    | None -> old_ancestors
    | Some addr -> addr :: old_ancestors
  in
  Ancestors.scope update @@ fun () ->
  render_tree ~opts tree

and render_internal_link ~title ~modifier ~addr ~dest =
  let url = E.route addr in
  let ancestors = Ancestors.read () in
  let dest_title =
    dest.fm.title |> Option.map @@
    Render_util.expand_title_with_parents ~ancestors dest.fm
  in
  let target_title_attr =
    match dest_title with
    | None -> F.null_
    | Some t ->
      let title_string =
        String_util.sentence_case @@
        Render_text.Printer.contents @@
        Render_text.render t
      in
      F.title_ "%s" title_string
  in
  let title =
    title
    |> Option.fold ~none:dest_title ~some:Option.some
    |> Option.map (Sem.apply_modifier modifier)
    |> Option.value ~default:[Range.locate_opt None @@ Sem.Text addr]
  in
  F.link [
    F.href "%s" url;
    F.type_ "local";
    F.addr_ "%s" addr
  ] [render_nodes title]

and render_external_link ~title ~modifier ~url =
  let title =
    title
    |> Option.map (Sem.apply_modifier modifier)
    |> Option.value ~default:[Range.locate_opt None @@ Sem.Text url]
  in
  F.link [
    F.href "%s" url;
    F.type_ "external"
  ] [render_nodes title]

and render_author_name author =
  let exception Untitled in
  try
    match E.get_doc author with
    | None -> raise Untitled
    | Some biotree ->
      match biotree.fm.addr with
      | None -> raise Untitled
      | Some addr ->
        let url = E.route addr in
        F.link [
          F.href "%s" url;
          F.type_ "local";
          F.addr_ "%s" addr
        ] [
          match biotree.fm.title with
          | None -> raise Untitled
          | Some title -> render_nodes title
        ]
  with Untitled ->
    txt "%s" author

and render_author author =
  F.author [] [render_author_name author]

and render_contributor contributor =
  F.contributor [] [render_author_name contributor]

and render_authors ~contributors ~authors =
  match authors, contributors with
  | [], [] -> F.null []
  | _, _ ->
    F.authors [] [
      F.null @@ List.map render_author authors;
      F.null @@ List.map render_contributor contributors
    ]

and render_meta (key, body) =
  F.meta [F.name "%s" key] [
    render_nodes body
  ]

and render_last_changed (fm : Sem.frontmatter) =
  match fm.addr with
  | Some addr ->
    let date = E.last_changed addr in
    date |> optional @@ fun date -> F.last_changed [] [render_date date]
  | None -> F.null []

and render_frontmatter ~opts (fm : Sem.frontmatter) =
  let anchor = string_of_int @@ Oo.id (object end) in
  let contributors =
    match fm.addr with
    | Some addr -> E.contributors addr
    | None -> []
  in
  let authors = fm.authors in

  F.frontmatter [] [
    F.anchor [] "%s" anchor;

    begin
      let taxon =
        match Sem.(opts.taxon_override) with
        | Some taxon -> Some taxon
        | None -> fm.taxon
      in
      match taxon with
      | None -> F.null []
      | Some taxon -> F.taxon [] "%s" @@ String_util.sentence_case taxon
    end;

    begin
      fm.addr |> optional @@ fun addr ->
      F.null [
        F.addr [] "%s" addr;
        F.route [] "%s" @@ E.route addr
      ]
    end;

    begin
      fm.source_path |> optional @@ fun path ->
      F.source_path [] "%s" path
    end;

    render_title ~opts fm;
    render_dates fm.dates;
    render_authors ~contributors ~authors;
    fm.number |> optional @@ F.number [] "%s";
    fm.designated_parent |> optional @@ F.parent [] "%s";
    F.null @@ List.map render_meta fm.metas;
    render_last_changed fm
  ]

and render_mainmatter nodes =
  F.mainmatter [] [render_nodes nodes]

and render_backmatter (addr : addr) =
  let opts =
    {Sem.default_transclusion_opts with
     numbered = false}
  in
  let render_trees = List.map (render_tree ~opts) in
  F.backmatter [] [
    F.contributions [] @@ render_trees @@ E.contributions addr;
    F.context [] @@ render_trees @@ E.parents addr;
    F.related [] @@ render_trees @@ E.related addr;
    F.backlinks [] @@ render_trees @@ E.backlinks addr;
    F.references [] @@ render_trees @@ E.bibliography addr
  ]

and render_title ~opts (fm : Sem.frontmatter) =
  let ancestors = Ancestors.read () in
  let title =
    match opts.title_override with
    | Some title -> Some title
    | None ->
      fm.title |> Option.map @@
      Render_util.expand_title_with_parents ~ancestors fm
  in
  title |> optional @@ fun title ->
  F.title [] [render_nodes @@ Sem.sentence_case title]

and render_tree ?(backmatter = false) ~opts (tree : Sem.tree) =
  Current_addr.run ~env:tree.fm.addr @@ fun  () ->
  let ancestors = Ancestors.read () in
  F.register_ns F.tree [
    F.toc opts.toc;
    F.numbered opts.numbered;
    F.show_heading opts.show_heading;
    F.show_metadata opts.show_metadata;
    F.expanded opts.expanded;
    F.root @@ Option.value ~default:false @@ Option.map E.is_root tree.fm.addr
  ] [
    render_frontmatter ~opts tree.fm;
    begin
      match tree.fm.addr with
      | None ->
        render_mainmatter tree.body
      | Some addr when List.mem addr ancestors ->
        F.mainmatter [] [
          F.prim `P [] [
            F.info [] [txt "Transclusion cycle"]
          ]
        ]
      | Some addr ->
        let cache = Mainmatter_cache.get () in
        match String_map.find_opt addr cache with
        | Some cached -> cached
        | None ->
          let result = render_mainmatter tree.body in
          Mainmatter_cache.modify (String_map.add addr result);
          result
    end;
    match backmatter, tree.fm.addr with
    | true, Some addr -> render_backmatter addr
    | _ -> F.null []
  ]

let render_tree_top (tree : Sem.tree) =
  Ancestors.run ~env:[] @@ fun () ->
  Current_addr.run ~env:None @@ fun () ->
  let env = Xmlns_map.assoc ~prefix:F.reserved_prefix ~xmlns:F.forester_xmlns Xmlns_map.empty in
  Xmlns_prefixes.run ~env @@ fun () ->
  render_tree ~backmatter:true ~opts:Sem.default_transclusion_opts tree

let with_mainmatter_cache kont =
  Mainmatter_cache.run ~init:String_map.empty kont
