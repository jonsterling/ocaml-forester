type t = Trie.path * int

val pp : Format.formatter -> t -> unit

val fresh : Trie.path -> t

val compare : t -> t -> int
