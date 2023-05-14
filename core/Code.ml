open Prelude
open Base

type node =
  | Text of string
  | Group of delim * t
  | Math of math_mode * t
  | Ident of string
  | Transclude of transclusion_mode * string
  | EmbedTeX of t
  | Let of string * string list * t
  | Block of t * t
[@@deriving show]

and t = node list
[@@deriving show]

type binder = string list * t
[@@deriving show]

type decl =
  | Import of visibility * addr
  | Def of Trie.path * binder
[@@deriving show]

type frontmatter =
  {title : t option;
   taxon : string option;
   authors : addr list;
   tags : addr list;
   date: Date.t option;
   metas : (string * t) list;
   decls : decl list;
   tex_packages : string list}
[@@deriving show]

type doc = frontmatter * t

let inline_math e = Math (Inline, e)
let display_math e = Math (Display, e)
let parens e = Group (Parens, e)
let squares e = Group (Squares, e)
let braces e = Group (Braces, e)
