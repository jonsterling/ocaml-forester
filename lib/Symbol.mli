type uid
[@@deriving show]

type t = 
  | User of string 
  | Machine of uid
[@@deriving show]

val fresh : unit -> uid 

val compare : t -> t -> int
