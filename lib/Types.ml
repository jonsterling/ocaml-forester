type addr = string

type renderer = Format.formatter -> unit

module Syn =
struct
  type t = 
    | Text of string 
    | Transclude of addr
    | Wikilink of t * addr
    | Tag of string * t list
    | Math of t
    | Seq of t list
    | Title of t 
    | Import of addr 
    | DefMacro of string * string list * t

  and attr = string * string
end

module Sem = 
struct 
  type t = 
    | Text of string 
    | Transclude of addr 
    | Wikilink of t * addr 
    | Tag of string * attr list * t list
    | Seq of t list
    | Math of t

  and attr = string * string

  type doc = 
    {title : t; 
     body : t}
end

module Map = Map.Make (String)

type env = Sem.t Map.t
type clo = Clo of env * string list * Syn.t
