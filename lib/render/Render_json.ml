open Forester_prelude
open Forester_core
open Sem

module E = Render_effect.Perform

let render_tree ~dev (doc : Sem.tree) =
  let addr = doc.fm.addr in
  let title =
    match doc.fm.title with
    | None -> `Null
    | Some title ->
      let title = Render_util.expand_title_with_parents doc.fm title in
      let title_string =
        String.trim @@
        String_util.sentence_case @@
        Render_text.Printer.contents @@
        Render_text.render title
      in
      `String title_string
  in
  let
    taxon =
    match doc.fm.taxon with
    | None -> `Null
    | Some taxon -> `String (String_util.sentence_case taxon)
  in
  let tags = `List (List.map (fun t -> `String t) doc.fm.tags) in
  let route = `String (E.route addr) in
  let metas =
    let meta_string meta =
      String.trim @@
      String_util.sentence_case @@
      Render_text.Printer.contents @@
      Render_text.render meta
    in
    `Assoc
      (List.map (fun (s, meta) -> (s, `String (meta_string meta)))
         doc.fm.metas)
  in
  let
    path =
    if dev then
      match doc.fm.source_path with
      | Some p -> [("sourcePath", `String p)]
      | None -> []
    else []
  in
  match addr with
  | User_addr addr ->
    Some
      (addr,
       `Assoc
         ( path @
           [("title", title);
            ("taxon", taxon);
            ("tags", tags);
            ("route",route);
            ("metas", metas);
           ]))
  | _ -> None

let render_trees ~(dev : bool) (docs : Sem.tree list) : Yojson.Basic.t =
  `Assoc (List.filter_map (render_tree ~dev) docs)

