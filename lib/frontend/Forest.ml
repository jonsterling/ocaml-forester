open Eio.Std
open Prelude
open Core
open Render

module A = Analysis
module M = A.Map
module Tbl = A.Tbl
module Gph = A.Gph

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   root : addr option;
   base_url : string option;
   ignore_tex_cache : bool;
   no_assets: bool;
   no_theme: bool;
   max_fibers : int}

type raw_forest = Code.tree Seq.t

type forest =
  {trees : Sem.tree Analysis.Map.t;
   analysis : Analysis.analysis Lazy.t}

module LaTeX_queue = LaTeX_queue.Make ()

let run_renderer ~cfg (forest : forest) (body : unit -> 'a) : 'a =
  let module S = Set.Make (String) in
  let module H : Render_effect.Handler =
  struct
    let analysis = Lazy.force forest.analysis

    let is_root addr =
      cfg.root = Some addr

    let route target addr =
      let ext =
        match target with
        | Render_effect.Xml -> "xml"
        | Render_effect.Rss -> "rss.xml"
      in
      let base =
        match is_root addr with
        | true -> "index"
        | false -> addr
      in
      Format.asprintf "%s.%s" base ext

    let get_doc addr =
      M.find_opt addr forest.trees

    let enqueue_latex ~name ~preamble ~source =
      LaTeX_queue.enqueue ~name ~preamble ~source

    let addr_peek_title scope =
      match M.find_opt scope forest.trees with
      | Some doc -> Sem.Util.peek_title doc
      | None -> None

    let get_sorted_trees addrs : Sem.tree list =
      let find addr =
        match M.find_opt addr forest.trees with
        | None -> []
        | Some doc -> [doc]
      in
      Sem.Util.sort @@ List.concat_map find @@ S.elements addrs

    let get_all_links scope =
      get_sorted_trees @@ S.of_list @@ Gph.pred analysis.link_graph scope

    let backlinks scope =
      get_sorted_trees @@ S.of_list @@ Gph.succ analysis.link_graph scope

    let related scope =
      get_all_links scope |> List.filter @@ fun (doc : Sem.tree) ->
      doc.fm.taxon <> Some "reference"

    let bibliography scope =
      get_sorted_trees @@
      S.of_list @@ A.Tbl.find_all analysis.bibliography scope

    let parents scope =
      get_sorted_trees @@ S.of_list @@ Gph.succ analysis.transclusion_graph scope

    let children scope =
      get_sorted_trees @@ S.of_list @@ Gph.pred analysis.transclusion_graph scope

    let contributions scope =
      get_sorted_trees @@ S.of_list @@ Tbl.find_all analysis.author_pages scope

    let contributors scope =
      let tree = M.find scope forest.trees in
      let authors = S.of_list tree.fm.authors in
      let contributors = S.union (S.of_list tree.fm.contributors) @@ S.of_list @@ Tbl.find_all analysis.contributors scope in
      let proper_contributors =
        contributors |> S.filter @@ fun contr ->
        not @@ S.mem contr authors
      in
      let by_title = Compare.under addr_peek_title @@ Compare.option String.compare in
      let compare = Compare.cascade by_title String.compare in
      List.sort compare @@ S.elements proper_contributors

    let run_query query =
      get_sorted_trees @@ S.of_seq @@ Seq.map fst @@ M.to_seq @@
      M.filter (fun _ -> Sem.Query.test query) forest.trees
  end
  in
  let module Run = Render_effect.Run (H) in
  Run.run body


let plant_forest (trees : raw_forest) : forest =
  let add_tree addr tree trees =
    if M.mem addr trees then
      begin
        Reporter.emitf Duplicate_tree "skipping duplicate tree at address `%s`" addr;
        trees
      end
    else
      M.add addr tree trees
  in

  let unexpanded_trees =
    let alg acc (tree : Code.tree) =
      match tree.addr with
      | Some addr -> add_tree addr tree acc
      | None -> acc
    in
    Seq.fold_left alg M.empty trees
  in

  let _, trees =
    let import_graph = A.build_import_graph trees in
    let task addr (units, trees) =
      let tree = M.find_opt addr unexpanded_trees in
      match tree with
      | None -> units, trees
      | Some tree ->
        let units, syn = Expand.expand_tree units tree in
        let tree, emitted_trees = Eval.eval_tree ~addr ~source_path:tree.source_path syn in
        let add trees tree =
          match Sem.(tree.fm.addr) with
          | None -> trees
          | Some addr -> add_tree addr tree trees
        in
        units, List.fold_left add trees @@ tree :: emitted_trees
    in
    A.Topo.fold task import_graph (Expand.UnitMap.empty, M.empty)
  in

  {trees; analysis = lazy (A.analyze_trees trees)}

let rec random_not_in keys =
  let attempt = Random.int (36*36*36*36 - 1) in
  if Seq.fold_left (fun x y -> x || y) false (Seq.map (fun k -> k == attempt) keys) then
    random_not_in keys
  else
    attempt

let next_addr ~prefix ~mode (forest : addr Seq.t) =
  let keys =
    forest |> Seq.filter_map @@ fun addr ->
    match String.split_on_char '-' addr with
    | [prefix'; str] when prefix' = prefix ->
      BaseN.Base36.int_of_string str
    | _ -> None
  in
  let next =
    match mode with
    | `Sequential -> 1 + Seq.fold_left max 0 keys
    | `Random -> random_not_in keys
  in
  prefix ^ "-" ^ BaseN.Base36.string_of_int next

let create_tree ~cfg ~addrs ~dest ~prefix ~template ~mode =
  let next = next_addr addrs ~prefix ~mode in
  let fname = next ^ ".tree" in
  let now = Date.now () in
  let template_content =
    match template with
    | None -> ""
    | Some name -> Eio.Path.load Eio.Path.(Eio.Stdenv.cwd cfg.env / "templates" / (name ^ ".tree"))
  in
  let body = Format.asprintf "\\date{%a}\n" Date.pp now in
  let create = `Exclusive 0o644 in
  let path = Eio.Path.(dest / fname) in
  Eio.Path.save ~create path @@ body ^ template_content;
  next

let complete ~forest prefix =
  forest.trees
  |> M.filter_map (fun _ -> Sem.Util.peek_title)
  |> M.filter (fun _ -> String.starts_with ~prefix)
  |> M.to_seq

let prefixes ~(addrs : addr Seq.t) : string list =
  let prefix addr =
    match String.split_on_char '-' addr with
    | [] | [_] -> None
    | prefix :: _ -> Some prefix
  in
  let prefixes =
    addrs
    |> Seq.map prefix
    |> Seq.filter_map Fun.id
    |> List.of_seq
    |> List.sort_uniq String.compare
  in
  prefixes

let taxa ~forest =
  forest.trees
  |> M.filter_map (fun _ -> Sem.Util.taxon)
  |> M.to_seq

let tags ~forest =
  forest.trees
  |> M.map Sem.Util.tags
  |> M.filter (fun _ -> fun tags -> not @@ List.is_empty tags)
  |> M.to_seq

module E = Render_effect.Perform

let render_tree ~cfg ~cwd doc =

  doc.fm.addr |> Option.iter @@ fun addr ->
  let create = `Or_truncate 0o644 in
  let base_url = cfg.base_url in
  begin
    (* TODO: the XML output via Eio is overflowing!!! *)
    let ch = open_out @@ "output/" ^ E.route Xml addr in
    (* let path = Eio.Path.(cwd / "output" / E.route Xml addr) in *)
    (* Eio.Path.with_open_out ~create path @@ fun flow -> *)
    (* Eio.Buf_write.with_flow flow @@ fun w -> *)
    Fun.protect ~finally:(fun _ -> close_out ch) @@ fun _ ->
    let out = Xmlm.make_output @@ `Channel ch in
    (* Eio_util.xmlm_dest_of_writer w in *)
    Render_xml.render_tree_page ~base_url doc out
  end

let render_json ~cwd docs =
  let create = `Or_truncate 0o644 in
  let json_path = Eio.Path.(cwd / "output" / "forest.json") in
  Eio.Path.with_open_out ~create json_path @@ fun json_sink ->
  Eio.Buf_write.with_flow json_sink @@ fun w ->
  let fmt = Eio_util.formatter_of_writer w in
  let docs = Sem.Util.sort_for_index @@ List.of_seq @@ Seq.map snd @@ M.to_seq docs in
  Render_json.render_trees docs fmt

let is_hidden_file fname =
  String.starts_with ~prefix:"." fname

let copy_theme ~env =
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  Eio.Path.with_open_dir Eio.Path.(fs / "theme") @@ fun theme ->
  Eio.Path.read_dir theme |> List.iter @@ fun fname ->
  if not @@ is_hidden_file fname then
    let source = "theme/" ^ fname in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"output"

let copy_assets ~env ~assets_dirs =
  let cwd = Eio.Stdenv.cwd env in
  assets_dirs |> List.iter @@ fun assets_dir ->
  Eio.Path.with_open_dir assets_dir @@ fun assets ->
  Eio.Path.read_dir assets |> List.iter @@ fun fname ->
  if not @@ is_hidden_file fname then
    let path = Eio.Path.(assets_dir / fname) in
    let source = Eio.Path.native_exn path in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"build";
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"output"

let copy_resources ~env =
  let cwd = Eio.Stdenv.cwd env in
  Eio.Path.with_open_dir Eio.Path.(cwd / "build") @@ fun build ->
  Eio.Path.read_dir build |> List.iter @@ fun fname ->
  if not @@ is_hidden_file fname then
    let ext = Filename.extension fname in
    let fp = Format.sprintf "build/%s" fname in
    let dest_opt =
      match ext with
      | ".svg" -> Some "output/resources"
      | _ -> None
    in
    dest_opt |> Option.iter @@ fun dest_dir ->
    if not @@ Eio_util.file_exists Eio.Path.(cwd / dest_dir / fname) then
      Eio_util.copy_to_dir ~cwd ~env ~source:fp ~dest_dir

let render_trees ~cfg ~forest : unit =
  let env = cfg.env in
  let cwd = Eio.Stdenv.cwd env in

  Eio_util.ensure_dir @@ Eio.Path.(cwd / "build");
  Eio_util.ensure_dir_path cwd ["output"; "resources"];

  run_renderer ~cfg forest @@ fun () ->
  forest.trees
  |> M.to_seq
  |> Seq.map snd
  |> List.of_seq
  |> Sem.Util.sort
  |> List.iter (render_tree ~cfg ~cwd);
  render_json ~cwd forest.trees;
  if not cfg.no_assets then
    copy_assets ~env ~assets_dirs:cfg.assets_dirs;
  if not cfg.no_theme then
    copy_theme ~env;
  let _ = LaTeX_queue.process ~env ~max_fibers:cfg.max_fibers ~ignore_tex_cache:cfg.ignore_tex_cache in
  copy_resources ~env
