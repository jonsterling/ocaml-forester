open Forester_core

val read_trees_in_dirs : dev:bool -> ?ignore_malformed:bool -> Eio.Fs.dir_ty Eio.Path.t list -> Code.tree list
