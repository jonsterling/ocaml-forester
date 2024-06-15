open Forester_prelude
open Forester_core

module Printer :
sig
  include Printer_kit.S with type out = Format.formatter
  val text : string -> t
  val contents : t -> string
end

val render : Sem.t -> Printer.t
