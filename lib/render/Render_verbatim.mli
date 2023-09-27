open Prelude
open Core

module Printer :
sig
  include Printer_kit.S with type out = Format.formatter
  val text : string -> t
  val contents : t -> string
end

type cfg = {tex : bool}

val render : cfg:cfg -> Sem.t -> Printer.t
