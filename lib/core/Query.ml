type 'a t = 
  | Author of 'a
  | Tag of 'a
  | Taxon of string
  | Meta of string * string
  | Or of 'a t list
  | And of 'a t list
  | Not of 'a t
  | True
[@@deriving show]
