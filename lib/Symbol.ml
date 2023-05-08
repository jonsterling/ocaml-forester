type uid = unit ref
[@@deriving show]

type t = 
  | User of string 
[@@deriving show]

let compare = compare
