module P =
struct
  type data = [`Term of Syn.t | `Sym of Symbol.t]
  type tag = unit

  type hook = unit (* for modifier hooks; unused here *)
  type context = unit (* for advanced printing and reporting; unused here *)
end

module Scope = Yuujinchou.Scope.Make (P)
module Lang = Yuujinchou.Language
