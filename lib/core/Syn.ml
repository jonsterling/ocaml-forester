open Base
open Prelude

type node =
  | Text of string
  | Group of delim * t
  | Math of math_mode * t
  | Tag of string
  | Link of {dest : t; title : t option}
  | Transclude of string
  | Query of t Query.t
  | Embed_tex of {packages : string list; source : t}
  | Block of t * t
  | Lam of Symbol.t list * t
  | Var of Symbol.t
  | Put of Symbol.t * t * t
  | Default of Symbol.t * t * t
  | Get of Symbol.t
  | If_tex of t * t
[@@deriving show]

and t = node list
[@@deriving show]

type frontmatter =
  {title : t option;
   addr : addr;
   taxon : string option;
   authors : addr list;
   tags : addr list;
   date: Date.t option;
   metas : (string * t) list;
   tex_packages : string list}
[@@deriving show]

type doc = frontmatter * t
