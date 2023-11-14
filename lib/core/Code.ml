open Prelude
open Base

type node =
  | Text of string
  | Group of delim * t
  | Math of math_mode * t
  | Ident of Trie.path * string list
  | Xml_tag of string * (string * t) list * t
  | Transclude of string
  | Embed_tex of t
  | Let of Trie.path * Trie.path list * t
  | Open of Trie.path
  | Block of t * t
  | Scope of t
  | Put of Trie.path * t
  | Default of Trie.path * t
  | Get of Trie.path
  | If_tex of t * t
  | Prim of Prim.t * t

  | Thunk of t
  | Force of t

  | Object of Trie.path option * (string * t) list
  | Patch of t * Trie.path option * (string * t) list
  | Call of t * string

  | Query of t Query.t

  | Import of visibility * addr
  | Def of Trie.path * Trie.path list * t
  | Alloc of Trie.path
  | Title of t
  | Taxon of string
  | Meta of string * t
  | Author of string
  | Tag of string
  | TeX_package of string
  | Date of string
  | Namespace of Trie.path * t
[@@deriving show]

and t = node Range.located list
[@@deriving show]

type doc = t
[@@deriving show]

let import_private x = Import (Private, x)
let import_public x = Import (Public, x)

let inline_math e = Math (Inline, e)
let display_math e = Math (Display, e)
let parens e = Group (Parens, e)
let squares e = Group (Squares, e)
let braces e = Group (Braces, e)
