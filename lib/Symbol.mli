type uid

type t = 
| User of string 
| Machine of uid

val fresh : unit -> uid 

val compare : t -> t -> int
