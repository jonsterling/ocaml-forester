open Eio.Std
open Forester_prelude
open Forester_core
open Forester_render

module A = Analysis
module M = A.Map
module Tbl = A.Tbl
module Gph = A.Gph

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   theme_dir : Eio.Fs.dir_ty Eio.Path.t;
   root : string option;
   base_url : string option;
   stylesheet : string;
   ignore_tex_cache : bool;
   no_assets: bool;
   no_theme: bool;
   max_fibers : int}

type raw_forest = Code.tree list

type forest =
  {trees : Sem.tree Analysis.Map.t;
   analysis : Analysis.analysis}

module LaTeX_queue = LaTeX_queue.Make ()

let run_renderer ~cfg (forest : forest) (body : unit -> 'a) : 'a =
  let module S = Set.Make (Addr) in
  let module H : Render_effect.Handler =
  struct
    let analysis = forest.analysis

    let is_root addr =
      Option.map (fun x -> User_addr x) cfg.root = Some addr

    let route addr =
      let ext = "xml" in
      let base =
        match is_root addr with
        | true -> "index"
        | false ->
          match addr with
          | User_addr addr -> addr
          | Machine_addr ix -> Format.sprintf "unstable-%i" ix
      in
      Format.asprintf "%s.%s" base ext

    let get_doc addr =
      M.find_opt addr forest.trees

    let enqueue_latex ~name ~preamble ~source =
      LaTeX_queue.enqueue ~name ~preamble ~source

    let addr_peek_title scope =
      Option.bind (M.find_opt scope forest.trees) Sem.Util.peek_title

    let get_sorted_trees addrs : Sem.tree list =
      let find addr =
        match M.find_opt addr forest.trees with
        | None -> []
        | Some doc -> [doc]
      in
      Sem.Util.sort @@ List.concat_map find @@ S.elements addrs

    let is_user_addr =
      function
      | User_addr _ -> true
      | _ -> false

    let get_all_links scope =
      get_sorted_trees @@ S.filter is_user_addr @@ S.of_list @@ Gph.pred analysis.link_graph scope

    let backlinks scope =
      get_sorted_trees @@ S.filter is_user_addr @@ S.of_list @@ Gph.succ analysis.link_graph scope

    let related scope =
      get_all_links scope |> List.filter @@ fun (doc : Sem.tree) ->
      doc.fm.taxon <> Some "reference"

    let bibliography scope =
      get_sorted_trees @@
      S.of_list @@ A.Tbl.find_all analysis.bibliography scope

    let parents scope =
      get_sorted_trees @@ S.of_list @@ Gph.succ analysis.transclusion_graph scope

    let contributions scope =
      get_sorted_trees @@ S.of_list @@ Tbl.find_all analysis.author_pages scope

    let contributors scope =
      try
        let tree = M.find scope forest.trees in
        let authors = S.of_list tree.fm.authors in
        let contributors = S.union (S.of_list tree.fm.contributors) @@ S.of_list @@ Tbl.find_all analysis.contributors scope in
        let proper_contributors =
          contributors |> S.filter @@ fun contr ->
          not @@ S.mem contr authors
        in
        let by_title = Compare.under addr_peek_title @@ Compare.option String.compare in
        (* let compare = Compare.cascade by_title String.compare in *)
        List.sort by_title @@ S.elements proper_contributors
      with Not_found -> []

    let run_query query =
      get_sorted_trees @@ S.of_seq @@ Seq.map fst @@ M.to_seq @@
      M.filter (fun _ -> Sem.Query.test query) forest.trees

    let last_changed scope =
      let (let*) = Option.bind in
      let* tree = M.find_opt scope forest.trees in
      let* source_path = tree.fm.source_path in
      let env = cfg.env in
      let path = Eio.Path.(Eio.Stdenv.fs env / source_path) in
      let stat  = Eio.Path.stat ~follow:true path in
      let* mtime = Some stat.mtime in
      let* ptime = Ptime.of_float_s mtime in
      let (yyyy, mm, dd) = ptime |> Ptime.to_date_time |> fst in
      Some (Date.{yyyy; mm = Some mm; dd = Some dd})
  end
  in
  let module Run = Render_effect.Run (H) in
  Run.run body


let plant_forest (trees : raw_forest) : forest =
  let add_tree addr tree trees =
    if M.mem addr trees then
      begin
        Reporter.emitf Duplicate_tree "skipping duplicate tree at address `%a`" pp_addr addr;
        trees
      end
    else
      M.add addr tree trees
  in

  let unexpanded_trees =
    let alg acc (tree : Code.tree) =
      match tree.addr with
      | Some addr -> add_tree (User_addr addr) tree acc
      | None -> acc
    in
    List.fold_left alg M.empty trees
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
          add_tree Sem.(tree.fm.addr) tree trees
        in
        units, List.fold_left add trees @@ tree :: emitted_trees
    in
    A.Topo.fold task import_graph (Expand.UnitMap.empty, M.empty)
  in

  {trees; analysis = A.analyze_trees trees}

let rec random_not_in keys =
  let attempt = Random.int (36*36*36*36 - 1) in
  if Seq.fold_left (fun x y -> x || y) false (Seq.map (fun k -> k = attempt) keys) then
    random_not_in keys
  else
    attempt

let split_addr addr =
  (* primitively check for address of form YYYY-MM-DD *)
  let date_regex = Str.regexp {|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$|} in
  if Str.string_match date_regex addr 0 then
    (addr, None)
  else
    match String.rindex_opt addr '-' with
    | Some i ->
      let prefix = String.sub addr 0 i
      and suffix = String.sub addr (i + 1) (String.length addr - i - 1) in
      begin
        match BaseN.Base36.int_of_string suffix with
        | Some key -> prefix, Some key
        | None -> addr, None
      end
    | _ -> addr, None

let next_addr ~prefix ~mode (forest : string Seq.t) =
  let keys =
    forest |> Seq.filter_map @@ fun addr ->
    let prefix', key = split_addr addr in
    if prefix = prefix' then key else None
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
  |> Seq.filter_map (fun (addr, x) -> Addr.to_user_addr addr |> Option.map (fun s -> s, x))

module Prefix_map = Map.Make (String)
let prefixes ~addrs =
  let cat_maybes  =
    List.fold_left
      (fun acc x -> match x with Some x -> x :: acc | None -> acc)
      []
  in
  let split_and_add_to_map map addr =
    let prefix, ix = split_addr addr in
    Prefix_map.add_to_list prefix ix map
  in
  addrs
  |> Seq.fold_left split_and_add_to_map Prefix_map.empty
  |> Prefix_map.map cat_maybes

let taxa ~forest =
  forest.trees
  |> M.filter_map (fun _ -> Sem.Util.taxon)
  |> M.to_seq
  |> Seq.filter_map (fun (addr, x) -> Addr.to_user_addr addr |> Option.map (fun s -> s, x))

let tags ~forest =
  forest.trees
  |> M.map Sem.Util.tags
  |> M.filter (fun _ -> fun tags -> not @@ List.is_empty tags)
  |> M.to_seq
  |> Seq.filter_map (fun (addr, x) -> Addr.to_user_addr addr |> Option.map (fun s -> s, x))


module E = Render_effect.Perform

let render_tree ~cfg ~cwd (tree : Sem.tree) =
  let addr = tree.fm.addr in
  let create = `Or_truncate 0o644 in
  let base_url = cfg.base_url in
  begin
    let path = Eio.Path.(cwd / "output" / E.route addr) in
    Eio.Path.with_open_out ~create path @@ fun flow ->
    Eio.Buf_write.with_flow flow @@ fun writer ->
    let fmt = Eio_util.formatter_of_writer writer in
    let node = Render_dream.render_tree_top tree in
    Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
    Format.pp_print_newline fmt ();
    Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" cfg.stylesheet;
    Format.pp_print_newline fmt ();
    Dream_html_lite.pp fmt node
  end

let render_json ~cwd docs =
  let docs = Sem.Util.sort_for_index @@ List.of_seq @@ Seq.map snd @@ M.to_seq docs in
  Yojson.Basic.to_file "./output/forest.json" @@
  Render_json.render_trees ~dev:false docs

let is_hidden_file fname =
  String.starts_with ~prefix:"." fname

let copy_theme ~env ~theme_dir =
  let cwd = Eio.Stdenv.cwd env in
  Eio.Path.read_dir theme_dir |> List.iter @@ fun fname ->
  if not @@ is_hidden_file fname then
    Eio.Path.native @@ Eio.Path.(theme_dir / fname) |> Option.iter @@ fun source ->
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"output"

let copy_assets ~env ~assets_dirs =
  let cwd = Eio.Stdenv.cwd env in
  assets_dirs |> List.iter @@ fun assets_dir ->
  Eio.Path.read_dir assets_dir |> List.iter @@ fun fname ->
  if not @@ is_hidden_file fname then
    let path = Eio.Path.(assets_dir / fname) in
    let source = Eio.Path.native_exn path in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"build";
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"output"

let copy_resources ~env =
  let cwd = Eio.Stdenv.cwd env in
  Eio.Path.read_dir Eio.Path.(cwd / "build") |> List.iter @@ fun fname ->
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

let render_trees ~cfg ~forest ~render_only : unit =
  let env = cfg.env in
  let cwd = Eio.Stdenv.cwd env in

  Eio_util.ensure_dir @@ Eio.Path.(cwd / "build");
  Eio_util.ensure_dir_path cwd ["output"; "resources"];

  run_renderer ~cfg forest @@ fun () ->
  Render_dream.with_mainmatter_cache @@ fun () ->
  let trees =
    match render_only with
    | None -> forest.trees |> M.to_seq |> Seq.map snd |> List.of_seq
    | Some addrs ->
      addrs |> List.map @@ fun addr ->
      match M.find_opt addr forest.trees with
      | Some tree -> tree
      | None ->
        Reporter.fatalf Tree_not_found "Could not find tree with address `%a` when rendering forest" pp_addr addr
  in
  trees
  |> Sem.Util.sort
  |> List.iter (render_tree ~cfg ~cwd);
  render_json ~cwd forest.trees;
  if not cfg.no_assets then
    copy_assets ~env ~assets_dirs:cfg.assets_dirs;
  if not cfg.no_theme then
    copy_theme ~env ~theme_dir:cfg.theme_dir;
  let _ = LaTeX_queue.process ~env ~max_fibers:cfg.max_fibers ~ignore_tex_cache:cfg.ignore_tex_cache in
  copy_resources ~env
