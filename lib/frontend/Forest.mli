open Core

module type S =
sig
  val plant_tree : sourcePath:string option -> addr -> Code.doc -> unit
  val render_trees : unit -> unit
end

module type I =
sig
  val size : int
  val root : addr option
  val base_url : string option
end

module Make (_ : I) : S
