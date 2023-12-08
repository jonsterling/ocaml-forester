open Base

type exports
module UnitMap : Map.S with type key = string

val expand_tree : exports UnitMap.t -> Code.tree -> exports UnitMap.t * Syn.tree

module Builtins :
sig
  module Transclude :
  sig
    val title_sym : Symbol.t
    val taxon_sym : Symbol.t
    val expanded_sym : Symbol.t
    val show_heading_sym : Symbol.t
    val show_metadata_sym : Symbol.t
    val toc_sym : Symbol.t
    val numbered_sym : Symbol.t
  end
end
