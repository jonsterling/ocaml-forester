type addr = string
[@@deriving show]

type delim = Braces | Squares | Parens
[@@deriving show]

type math_mode = Inline | Display
[@@deriving show]

type transclusion_mode = Full | Collapsed | Spliced
[@@deriving show]
