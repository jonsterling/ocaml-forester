open Base 
open Prelude

type attr = string * string
[@@deriving show]

type node = 
  | Text of string 
  | Transclude of transclusion_mode * addr 
  | Link of {dest : string; title : t}
  | Tag of string * attr list * t list
  | Math of math_mode * t
  | EmbedTeX of {packages : string list; source : t}
  | Block of t * t
[@@deriving show]

and t = node list

and env = t Env.t
[@@deriving show]

and clo = Clo of env * string list * Term.t
[@@deriving show]

let rec node_map_text (f : string -> string) : node -> node =
  function 
  | Text str -> Text (f str)
  | Link {dest; title} -> Link {title = map_text f title; dest}
  | Tag (tag, attrs, xs) -> Tag (tag, attrs, List.map (map_text f) xs)
  | node -> node

and map_text (f : string -> string) : t -> t =
  List.map @@ node_map_text f

let empty : env = Env.empty

type doc = 
  {title : t option;
   taxon : string option;
   authors : addr list;
   date: Date.t option;
   addr : addr;
   metas : (string * t) list;
   body : t}
[@@deriving show]
