module P =
struct
  type data =
    | Term of Syn.t
    | Sym of Symbol.t
    | Xmlns of {xmlns : string; prefix : string}
    | Tree_set of Base.addr list

  type tag = unit

  type hook = unit (* for modifier hooks; unused here *)
  type context = unit (* for advanced printing and reporting; unused here *)
end

module Scope = Yuujinchou.Scope.Make (P)
module Lang = Yuujinchou.Language
