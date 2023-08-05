module type I =
sig
  val alphabet : string
end

module type S =
sig
  val base : int
  val int_of_string : string -> int option
  val string_of_int : int -> string
end

module Make (_ : I) : S
module Base36 : S
