open Core

module type S =
sig
  val plant_tree : sourcePath:string option -> addr -> Code.doc -> unit
  val create_tree : dir:string -> prefix:string -> addr
  val render_trees : unit -> unit
end

module type I =
sig
  val env : Eio_unix.Stdenv.base

  val root : addr option
  val base_url : string option
  val ignore_tex_cache : bool
  val max_fibers : int
end

module Make (_ : I) : S
