open Base
open Prelude

type node =
  | Text of string
  | Verbatim of string
  | Group of delim * t
  | Math of math_mode * t
  | Link of {dest : t; title : t option}
  | Transclude of t
  | Subtree of string option * tree
  | Query of t Query.t
  | Embed_tex of {preamble : t; source : t}
  | Lam of Symbol.t list * t
  | Var of Symbol.t
  | Put of Symbol.t * t * t
  | Default of Symbol.t * t * t
  | Get of Symbol.t
  | If_tex of t * t
  | Xml_tag of xml_resolved_qname * (xml_resolved_qname * t) list * t
  | Unresolved of string
  | Prim of Prim.t * t
  | Object of {self : Symbol.t; methods: (string * t) list}
  | Patch of {obj : t; self : Symbol.t; super : Symbol.t; methods : (string * t) list}
  | Call of t * string
  | Ref of t

  | Title of t
  | Parent of string
  | Taxon of string
  | Meta of string * t
  | Author of string
  | Contributor of string
  | Tag of string
  | Date of string
  | Number of string

[@@deriving show]

and t = node Range.located list
[@@deriving show]

and tree = t

