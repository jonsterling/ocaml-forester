type addr = User_addr of string | Machine_addr of int

let pp_addr fmt =
  function
  | User_addr str -> Format.pp_print_string fmt str
  | Machine_addr ix -> Format.fprintf fmt "#%i" ix

module Addr =
struct
  type t = addr
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)

  let to_user_addr =
    function
    | User_addr addr -> Some addr
    | _ -> None
end

type delim = Braces | Squares | Parens
[@@deriving show]

type math_mode = Inline | Display
[@@deriving show]

type visibility = Private | Public
[@@deriving show]

type xml_resolved_qname =
  | Xml_resolved_qname of {prefix : string option; uname : string; xmlns : string option}
[@@deriving show]
