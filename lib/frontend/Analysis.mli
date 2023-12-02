open Core

module Gph : sig
  type t
  val succ : t -> addr -> addr list
  val pred : t -> addr -> addr list
end

module Tbl : sig
  type 'a t
  val find_all : 'a t -> addr -> 'a list
end

module Map : Map.S with type key = addr

module Topo : sig
  val fold : (addr -> 'a -> 'a) -> Gph.t -> 'a -> 'a
end

module type S =
sig
  (** Populating the import graph by inspecting unexpanded trees *)
  val plant_tree : addr -> Code.t -> unit

  (** Populating all the graphs and tables by inspecting evaluated trees *)
  val analyze_trees : Sem.doc Map.t -> unit

  val transclusion_graph : Gph.t
  val link_graph : Gph.t
  val import_graph : Gph.t
  val contributors : addr Tbl.t
  val author_pages : addr Tbl.t
  val bibliography : addr Tbl.t
end

module Make () : S
