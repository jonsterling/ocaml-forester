type t = {yyyy : int; mm : int option; dd : int option}

val pp : Format.formatter -> t -> unit
val pp_human : Format.formatter -> t -> unit
val parse : string -> t
val now : unit -> t

val compare : t -> t -> int

val year : t -> int
val month : t -> int option
val day : t -> int option
