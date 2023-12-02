open Core

module Gph : Graph.Sig.I with type V.t = addr
module Tbl : Hashtbl.S with type key = addr
module Map : Map.S with type key = addr

module type S =
sig
  val plant_tree : addr -> Code.t -> unit
  val analyze_trees : Sem.doc Map.t -> unit

  val transclusion_graph : Gph.t
  val link_graph : Gph.t
  val contributors : addr Tbl.t
  val author_pages : addr Tbl.t
  val bibliography : addr Tbl.t
  val import_graph : Gph.t
end

module Make () : S
