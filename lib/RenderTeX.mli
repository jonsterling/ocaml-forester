open Types 

module Printer : 
sig
  type t
  val contents : t -> string 
end

val render_nodes : Sem.t -> Printer.t
