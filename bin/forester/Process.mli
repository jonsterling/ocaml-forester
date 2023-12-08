open Forester
open Core

type tree =
  {source_path : string option;
   addr : addr;
   code : Code.t}

val read_trees_in_dir : dev:bool -> string -> tree Seq.t
val read_trees_in_dirs : dev:bool -> string list -> tree Seq.t
