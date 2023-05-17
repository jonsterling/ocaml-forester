open Base
open Prelude

type attr = string * string
[@@deriving show]

type node =
  | Text of string
  | Transclude of int * transclusion_mode * addr
  | Link of {dest : string; title : t}
  | Tag of string * attr list * t list
  | Math of math_mode * t
  | EmbedTeX of {packages : string list; source : t}
  | Block of t * t
[@@deriving show]

and t = node list

and env = t Env.t
[@@deriving show]

and clo = Clo of env * string list * Syn.t
[@@deriving show]

let sentence_case =
  function 
  | Text str :: xs -> Text (StringUtil.sentence_case str) :: xs
  | xs -> xs

type doc =
  {title : t option;
   taxon : string option;
   authors : addr list;
   date: Date.t option;
   addr : addr;
   metas : (string * t) list;
   body : t}
[@@deriving show]
