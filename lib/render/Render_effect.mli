open Forester_prelude
open Forester_core

module type Handler =
sig
  val route : addr -> string
  val is_root : addr -> bool
  val backlinks : addr -> Sem.tree list
  val related : addr -> Sem.tree list
  val bibliography : addr -> Sem.tree list
  val parents : addr -> Sem.tree list
  val contributors : addr -> addr list
  val contributions : addr -> Sem.tree list
  val enqueue_latex : name:string -> preamble:string -> source:string -> unit
  val get_doc : addr -> Sem.tree option
  val run_query : Sem.t Query.t -> Sem.tree list
  val last_changed : addr -> Date.t option
end

module Perform : Handler

module Run (_ : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end
