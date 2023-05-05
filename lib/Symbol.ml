type uid = unit ref
[@@deriving show]

type t = 
  | User of string 
  | Machine of uid
[@@deriving show]

let fresh _ = 
  ref ()

let compare = compare
