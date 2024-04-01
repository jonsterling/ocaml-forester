open Eio.Std
open Forester
open Core
open Cmdliner

module Tty = Asai.Tty.Make (Core.Reporter.Message)

module Forest_config =
struct
  type t =
    {trees : string list;
     assets : string list;
     theme : string;
     base_url : string option;
     root : addr option}
  [@@deriving show]
end

let default_forest_config : Forest_config.t =
  {trees = ["trees"];
   assets = [];
   theme = "theme";
   base_url = None;
   root = None}

let parse_forest_config contents =
  match Toml.Parser.from_string contents with
  | `Error (msg, location) ->
    Reporter.emit Parse_error msg;
    default_forest_config
  | `Ok tbl ->
    let open Toml.Lenses in
    let forest = key "forest" |-- table in
    let trees =
      Option.value ~default:default_forest_config.trees @@
      get tbl (forest |-- key "trees" |-- array |-- strings)
    in
    let assets =
      Option.value ~default:default_forest_config.assets @@
      get tbl (forest |-- key "assets" |-- array |-- strings)
    in
    let theme =
      Option.value ~default:default_forest_config.theme @@
      get tbl (forest |-- key "theme" |-- string)
    in
    let base_url = get tbl (forest |-- key "base_url" |-- string) in
    let root = get tbl (forest |-- key "root" |-- string) in
    Forest_config.{assets; trees; theme; base_url; root}

let get_forest_config env config_filename =
  let fs = Eio.Stdenv.fs env in
  match Eio.Path.(load (fs / config_filename)) with
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
    default_forest_config
  | exception _ -> failwith "fuck"
  | contents ->
    parse_forest_config contents


let make_dir ~env dir =
  Eio.Path.(Eio.Stdenv.fs env / dir)

let make_dirs ~env =
  List.map (make_dir ~env)

let internal_config_from_config ~env (config : Forest_config.t) =
  Forest.
    {env;
     root = config.root;
     base_url = config.base_url;
     assets_dirs = make_dirs ~env config.assets;
     theme_dir = make_dir ~env config.theme;
     max_fibers = 20;
     ignore_tex_cache = false;
     no_assets = true;
     no_theme = true}

let version =
  Format.asprintf "%s" @@
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v


let build ~env config_filename dev ignore_tex_cache no_assets no_theme =
  let config = get_forest_config env config_filename in
  let internal_cfg = internal_config_from_config ~env config in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev @@
    make_dirs ~env config.trees
  in
  Forest.render_trees ~cfg:internal_cfg ~forest

let new_tree ~env config_filename dest_dir prefix template random =
  let config = get_forest_config env config_filename in
  let internal_config =
    {(internal_config_from_config ~env config) with
     no_assets = true;
     no_theme = true}
  in
  let input_dirs = make_dirs ~env config.trees in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true input_dirs
  in
  let addrs =
    Analysis.Map.bindings forest.trees
    |> List.to_seq
    |> Seq.map fst
  in
  let mode = if random then `Random else `Sequential in
  let addr = Forest.create_tree ~cfg:internal_config ~dest:(make_dir ~env dest_dir) ~prefix ~template ~addrs ~mode in
  Format.printf "%s/%s.tree\n" dest_dir addr

let complete ~env config_filename title =
  let config = get_forest_config env config_filename in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  let completions = Forest.complete ~forest title in
  completions |> Seq.iter @@ fun (addr, title) ->
  Format.printf "%s, %s\n" addr title

let query_prefixes ~env config_filename =
  let config = get_forest_config env config_filename in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  let addrs =
    Analysis.Map.bindings forest.trees
    |> List.to_seq
    |> Seq.map fst
  in
  let prefixes = Forest.prefixes ~addrs in
  prefixes |> List.iter @@ fun addr ->
  Format.printf "%s\n" addr

let query_taxon ~env taxon config_filename =
  let config = get_forest_config env config_filename in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  let taxa = Forest.taxa ~forest in
  taxa |> Seq.iter @@ fun (addr, taxon) ->
  Format.printf "%s, %s\n" addr taxon

let query_tag ~env config_filename =
  let config = get_forest_config env config_filename in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  let tags = Forest.tags ~forest in
  tags |> Seq.iter @@ fun (addr, tags) ->
  let rec tag_string = function
    | [] -> ""
    | hd :: tl -> ", " ^ hd ^ tag_string tl
  in
  Format.printf "%s%s\n" addr (tag_string tags)

let query_all ~env config_filename =
  let config = get_forest_config env config_filename in
  let (forest : Forest.forest) =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  let internal_config = internal_config_from_config ~env config in
  Forest.run_renderer ~cfg:internal_config forest @@ fun () ->
  forest.trees
  |> Analysis.Map.to_list
  |> List.map snd
  |> Render.Render_json.render_trees ~dev:true
  |> Yojson.Basic.to_string
  |> Format.printf "%s"


let arg_config =
  let doc = "A TOML file like $(i,forest.toml)" in
  Arg.(value & pos 0 file "forest.toml" & info [] ~docv:"MSG" ~doc)

let build_cmd ~env =
  let arg_dev =
    let doc = "Run forester in development mode; this will attach source file locations to the generated json." in
    Arg.value @@ Arg.flag @@ Arg.info ["dev"] ~doc
  in

  let arg_ignore_tex_cache =
    let doc = "Ignore the SVG cache when building LaTeX assets." in
    Arg.value @@ Arg.flag @@ Arg.info ["ignore-tex-cache"] ~doc
  in

  let arg_no_assets =
    let doc = "Build without copying the asset directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-assets"] ~doc
  in

  let arg_no_theme =
    let doc = "Build without copying the theme directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-theme"] ~doc
  in

  let doc = "Build the forest" in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command builds a hypertext $(b,forest) from trees stored in each $(i,INPUT_DIR) or any of its subdirectories; tree files are expected to be of the form $(i,addr.tree) where $(i,addr) is the global address of the tree. Note that the physical location of a tree is not taken into account, and two trees with the same address are not permitted.";
  ]
  in
  let info = Cmd.info "build" ~version ~doc ~man in
  Cmd.v info
    Term
    .(const (build ~env)
      $ arg_config
      $ arg_dev
      $ arg_ignore_tex_cache
      $ arg_no_assets
      $ arg_no_theme)

let new_tree_cmd ~env =
  let arg_prefix =
    let doc = "The namespace prefix for the created tree." in
    Arg.required @@ Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["prefix"] ~docv:"XXX" ~doc
  in
  let arg_template =
    let doc = "The tree to use as a template" in
    Arg.value @@ Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["template"] ~docv:"XXX" ~doc
  in
  let arg_dest_dir =
    let doc = "The directory in which to deposit created tree." in
    Arg.required @@ Arg.opt (Arg.some Arg.dir) None @@
    Arg.info ["dest"] ~docv:"DEST" ~doc
  in
  let arg_random =
    let doc = "True if the new tree should have id assigned randomly rather than sequentially" in
    Arg.value @@ Arg.flag @@ Arg.info ["random"] ~doc
  in
  let doc = "Create a new tree." in
  let info = Cmd.info "new" ~version ~doc in
  Cmd.v info
    Term
    .(const (new_tree ~env)
      $ arg_config
      $ arg_dest_dir
      $ arg_prefix
      $ arg_template
      $ arg_random)

let complete_cmd ~env =
  let arg_title =
    let doc = "The tree title prefix to complete." in
    Arg.value @@ Arg.opt Arg.string "" @@
    Arg.info ["title"] ~docv:"title" ~doc
  in
  let doc = "Complete a tree title." in
  let info = Cmd.info "complete" ~version ~doc in
  Cmd.v info Term.(const (complete ~env) $ arg_config $ arg_title)


let query_prefixes_cmd ~env =
  let doc = "Get all prefixes of a forest" in
  let info = Cmd.info "prefix" ~version ~doc in
  Cmd.v info Term.(const (query_prefixes ~env) $ arg_config)

let query_taxon_cmd ~env =
  let arg_taxon =
    Arg.non_empty @@ Arg.pos_all Arg.file [] @@
    Arg.info [] ~docv:"TAXON"
  in
  let doc = "List all trees of taxon TAXON" in
  let info = Cmd.info "taxon" ~version ~doc in
  Cmd.v info Term.(const (query_taxon ~env) $ arg_taxon $ arg_config)

let query_tag_cmd ~env =
  let doc = "List all trees with tag TAG" in
  let info = Cmd.info "tag" ~version ~doc in
  Cmd.v info Term.(const (query_tag ~env) $ arg_config)

let query_all_cmd ~env =
  let doc = "List all trees in JSON format" in
  let info = Cmd.info "all" ~version ~doc in
  Cmd.v info Term.(const (query_all ~env) $ arg_config)

let query_cmd ~env =
  let doc = "Query your forest" in
  let info = Cmd.info "query" ~version ~doc in
  Cmd.group info [query_prefixes_cmd ~env; query_taxon_cmd ~env; query_tag_cmd ~env; query_all_cmd ~env]


let cmd ~env =
  let doc = "a tool for tending mathematical forests" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <~jonsterling/forester-discuss@lists.sr.ht>." ;
    `S Manpage.s_authors;
    `P "Jonathan Sterling"
  ]
  in

  let info = Cmd.info "forester" ~version ~doc ~man in
  Cmd.group info [build_cmd ~env; new_tree_cmd ~env; complete_cmd ~env; query_cmd ~env]


let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  exit @@ Cmd.eval ~catch:false @@ cmd ~env
