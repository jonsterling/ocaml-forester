open Base

type exports
module UnitMap : Map.S with type key = string

val expand_doc : exports UnitMap.t -> addr -> Code.doc -> exports UnitMap.t * Syn.doc
