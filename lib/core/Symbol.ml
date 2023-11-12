type t = Trie.path * int

let counter = ref 0

let fresh path =
  counter := !counter + 1;
  path, !counter

let pp fmt (sym, ix) =
  Format.fprintf fmt "%a%i" Trie.pp_path sym ix

let compare = compare
