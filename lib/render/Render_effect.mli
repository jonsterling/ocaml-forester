open Core

type target =
  | Xml
  | Rss

module type Handler =
sig
  val route : target -> addr -> string
  val source_path : addr -> string option
  val is_root : addr -> bool
  val backlinks : addr -> Sem.doc list
  val related : addr -> Sem.doc list
  val bibliography : addr -> Sem.doc list
  val parents : addr -> Sem.doc list
  val children : addr -> Sem.doc list
  val contributors : addr -> string list
  val contributions : addr -> Sem.doc list
  val enqueue_latex : name:string -> packages:string list -> source:string -> unit
  val get_doc : addr -> Sem.doc option
  val run_query : Sem.t Query.t -> Sem.doc list
end

module Perform : Handler

module Run (_ : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end
