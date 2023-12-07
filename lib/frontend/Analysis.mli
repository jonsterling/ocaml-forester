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

type analysis =
  {transclusion_graph : Gph.t;
   link_graph : Gph.t;
   contributors : addr Tbl.t;
   author_pages : addr Tbl.t;
   bibliography : addr Tbl.t}

module type S =
sig
  (** Populating the import graph by inspecting unexpanded trees *)
  val plant_tree : addr -> Code.t -> unit

  val import_graph : Gph.t

  (** Populating all the graphs and tables by inspecting evaluated trees *)
  val analyze_trees : Sem.doc Map.t -> analysis
end

module Make () : S
