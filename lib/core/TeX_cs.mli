type t =
  | Word of string
  | Symbol of char
[@@deriving show]

val parse : string -> (t * string) option
