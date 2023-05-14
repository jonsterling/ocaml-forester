include Yuujinchou.Trie

let pp_path =
  let pp_sep fmt () = Format.pp_print_string fmt "." in
  Format.pp_print_list ~pp_sep Format.pp_print_string
