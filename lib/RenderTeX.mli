open Types 

module Printer : 
sig
  include PrinterKit.S
  val text : string -> t
  val contents : t -> string 
end

val render_nodes : Sem.t -> Printer.t
