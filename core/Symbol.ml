type t = Trie.path ref

let fresh path = 
  ref path

let pp fmt sym =
  Format.fprintf fmt "%a" Trie.pp_path !sym

let compare = compare