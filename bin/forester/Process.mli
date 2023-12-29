open Forester
open Core

val read_trees_in_dirs : dev:bool -> ?ignore_malformed:bool -> Eio.Fs.dir_ty Eio.Path.t list -> Code.tree Seq.t
val read_addrs_in_dirs : Eio.Fs.dir_ty Eio.Path.t list -> addr Seq.t
