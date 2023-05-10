type t 

val pp : Format.formatter -> t -> unit
val pp_human : Format.formatter -> t -> unit 
val parse : string -> t

val compare : t -> t -> int