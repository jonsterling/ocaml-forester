module P =
struct
  type data =
    | Term of Syn.t
    | Sym of Symbol.t
    | Xmlns of {xmlns : string; prefix : string}

  type tag = unit

  type hook = unit (* for modifier hooks; unused here *)
  type context = unit (* for advanced printing and reporting; unused here *)
end

module Scope =
struct
  include Yuujinchou.Scope.Make (P)

  let import_singleton x v =
    import_singleton (x, (v, ()))

  let include_singleton x v =
    include_singleton (x, (v, ()))

  let import_subtree ?modifier path subtree =
    import_subtree ?modifier (path, subtree)

  let include_subtree ?modifier path subtree =
    include_subtree ?modifier (path, subtree)
end

module Lang = Yuujinchou.Language
