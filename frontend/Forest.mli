open Core

module type S =
sig
  val plant_tree : abspath:string option -> addr -> Code.doc -> unit
  val render_trees : unit -> unit
end

module type I = 
sig
  val size : int 
  val root : addr option
end

module Make (_ : I) : S
