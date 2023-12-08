type 'a t =
  | Author of 'a
  | Tag of 'a
  | Taxon of 'a
  | Meta of string * 'a
  | Or of 'a t list
  | And of 'a t list
  | Not of 'a t
  | True
[@@deriving show]

let rec map f =
  function
  | Author x -> Author (f x)
  | Tag x -> Tag (f x)
  | Taxon x -> Taxon (f x )
  | Meta (k, v) -> Meta (k, f v)
  | Or qs -> Or (List.map (map f) qs)
  | And qs -> And (List.map (map f) qs)
  | Not q -> Not (map f q)
  | True -> True

