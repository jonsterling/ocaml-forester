open Forester
open Core

val read_trees_in_dirs : dev:bool -> Eio.Fs.dir_ty Eio.Path.t list -> Code.tree Seq.t
