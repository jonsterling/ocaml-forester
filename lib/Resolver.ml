open Types 

open struct module Y = Yuujinchou end

module P =
struct
  type data = Term.t
  type tag = unit

  type hook = unit (* for modifier hooks; unused here *)
  type context = unit (* for advanced printing and reporting; unused here *)
end

module Mod = Y.Modifier.Make (P)
module Scope = Y.Scope.Make (P) (Mod)
