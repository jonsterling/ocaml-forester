open Core

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : string list;
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
val create_tree : cfg:config -> forest:raw_forest -> dir:string -> dest:string -> prefix:string -> template:string option -> mode:[`Sequential | `Random] -> addr

val complete : forest:forest -> string -> (addr * string) Seq.t
val prefixes : forest:forest -> string list
