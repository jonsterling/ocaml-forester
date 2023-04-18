type addr = string

type renderer = Format.formatter -> unit

module Syn =
struct
  type t = 
    | Text of string 
    | Transclude of addr
    | Wikilink of t * addr
    | Tag of string * attr list * t
    | Macro of string * t list
    | Seq of t list
    | BVar of int
    | Title of t 
    | Import of addr 
    | DefMacro of string * t

  and attr = string * string
end

module Sem = 
struct 
  type t = 
    | Text of string 
    | Transclude of addr 
    | Wikilink of t * addr 
    | Tag of string * attr list * t
    | Seq of t list

  and attr = string * string

  type doc = 
    {title : t; 
     body : t}
end
