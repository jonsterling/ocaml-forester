type uid = unit ref

type t = 
  | User of string 
  | Machine of uid

let fresh _ = 
  ref ()

let compare = compare
