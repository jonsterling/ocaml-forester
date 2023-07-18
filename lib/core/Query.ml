type 'a t = 
  | Author of string
  | Tag of string
  | Taxon of string
  | Meta of string * 'a
  | Or of 'a t list
  | And of 'a t list
  | Not of 'a t
  | True
[@@deriving show]

let rec map f = 
  function 
  | Author x -> Author x
  | Tag x -> Tag x
  | Taxon x -> Taxon x 
  | Meta (k, v) -> Meta (k, f v)
  | Or qs -> Or (List.map (map f) qs)
  | And qs -> And (List.map (map f) qs)
  | Not q -> Not (map f q)
  | True -> True
