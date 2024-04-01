type addr = string
[@@deriving show]

type delim = Braces | Squares | Parens
[@@deriving show]

type math_mode = Inline | Display
[@@deriving show]

type visibility = Private | Public
[@@deriving show]

type xml_resolved_qname =
  | Xml_resolved_qname of {prefix : string option; uname : string; xmlns : string option}
[@@deriving show]
