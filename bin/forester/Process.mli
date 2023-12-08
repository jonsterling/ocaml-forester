open Forester
open Core

val read_trees_in_dir : dev:bool -> string -> Code.tree Seq.t
val read_trees_in_dirs : dev:bool -> string list -> Code.tree Seq.t
