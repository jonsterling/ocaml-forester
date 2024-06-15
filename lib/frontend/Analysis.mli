open Forester_core

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

val build_import_graph : Code.tree list -> Gph.t

type analysis =
  {transclusion_graph : Gph.t;
   link_graph : Gph.t;
   contributors : addr Tbl.t;
   author_pages : addr Tbl.t;
   bibliography : addr Tbl.t}

(** Populating all the graphs and tables by inspecting evaluated trees *)
val analyze_trees : Sem.tree Map.t -> analysis
