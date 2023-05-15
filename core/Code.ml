open Prelude
open Base

type node =
  | Text of string
  | Group of delim * t
  | Math of math_mode * t
  | Ident of Trie.path
  | Transclude of transclusion_mode * string
  | EmbedTeX of t
  | Let of Trie.path * string list * t
  | Block of t * t
  | Scope of t
  | Put of Trie.path * t 
  | Default of Trie.path * t
  | Get of Trie.path
[@@deriving show]

and t = node list
[@@deriving show]

and binder = string list * t
[@@deriving show]

type decl =
  | Import of visibility * addr
  | Def of Trie.path * string list * t
  | Alloc of Trie.path
  | Title of t 
  | Taxon of string
  | Meta of string * t 
  | Author of string 
  | Tag of string 
  | TeXPackage of string
  | Date of string
  | Namespace of Trie.path * decl list
[@@deriving show]

type frontmatter = decl list
[@@deriving show]

type doc = frontmatter * t
[@@deriving show]

let import_private x = Import (Private, x)
let import_public x = Import (Public, x)

let inline_math e = Math (Inline, e)
let display_math e = Math (Display, e)
let parens e = Group (Parens, e)
let squares e = Group (Squares, e)
let braces e = Group (Braces, e)
