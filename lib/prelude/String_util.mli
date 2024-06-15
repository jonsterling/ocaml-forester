open Bwd

val sentence_case : string -> string
val trim_newlines : string -> string
val trim_trailing_whitespace : string -> string

val explode : string -> char list
val implode : char list -> string
val implode_bwd : char bwd -> string
