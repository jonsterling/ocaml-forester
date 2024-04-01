open Core

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   theme_dir : Eio.Fs.dir_ty Eio.Path.t;
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

val plant_forest : Code.tree Seq.t -> forest
val render_trees : cfg:config -> forest:forest -> unit
val create_tree : cfg:config -> addrs:addr Seq.t -> dest:Eio.Fs.dir_ty Eio.Path.t -> prefix:string -> template:string option -> mode:[`Sequential | `Random] -> addr

val complete : forest:forest -> string -> (addr * string) Seq.t
val prefixes : addrs:addr Seq.t -> string list
val taxa : forest:forest-> (addr * string) Seq.t
val tags : forest:forest -> (addr * string list) Seq.t
val run_renderer : cfg:config -> forest -> ( unit -> 'a) -> 'a
val render_json : cwd:[> Eio__.Fs.dir_ty ] Eio.Path.t -> Core.Sem.tree Analysis.Map.t -> unit
