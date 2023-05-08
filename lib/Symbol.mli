type t = 
  | User of string 
[@@deriving show]

val compare : t -> t -> int
