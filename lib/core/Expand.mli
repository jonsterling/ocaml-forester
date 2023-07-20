open Base

type exports
module UnitMap : Map.S with type key = string

val expand_doc : exports UnitMap.t -> addr -> Code.doc -> exports UnitMap.t * Syn.doc

module Builtins : 
sig
  module Transclude :
  sig
    val title_sym : Symbol.t
    val expanded_sym : Symbol.t
    val show_heading_sym : Symbol.t
    val toc_sym : Symbol.t
    val numbered_sym : Symbol.t
  end 
end
