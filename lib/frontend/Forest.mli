open Core

module type S =
sig
  val plant_trees : Code.tree Seq.t -> Analysis.Gph.t
  val render_trees : import_graph:Analysis.Gph.t -> unit
  val create_tree : import_graph:Analysis.Gph.t -> dir:string -> dest:string -> prefix:string -> template:string option -> addr
  val complete : import_graph:Analysis.Gph.t -> string -> (addr * string) Seq.t
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
