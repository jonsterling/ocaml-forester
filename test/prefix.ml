open Forester

(* pretty-print an address prefix, ignoring the index *)
let pp_print_prefix formatter (prefix, _index) =
  Format.pp_print_string formatter prefix

let _ =
  let addrs = List.to_seq [ "foo"; "bar-0001"; "foo-bar-0002"; "2024-05-08"] in
  let prefixes = Forest.prefixes ~addrs in
  Forest.Prefix_map.to_list prefixes
  |> Format.printf "prefixes: @[<hov>%a@]@."
  @@ Format.pp_print_list ~pp_sep:Format.pp_print_space pp_print_prefix
