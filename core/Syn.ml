open Base
open Prelude

type node =
  | Text of string
  | Group of delim * t
  | Math of math_mode * t
  | Tag of string
  | Link of {dest : string; title : t}
  | Transclude of transclusion_mode * string
  | EmbedTeX of {packages : string list; source : t}
  | Block of t * t
  | Lam of string list * t
  | Var of string
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
