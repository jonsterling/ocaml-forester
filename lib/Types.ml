type addr = string

type renderer = Format.formatter -> unit

module Syn =
struct
  type t = 
    | Text of string 
    | Transclude of addr
    | Wikilink of t * addr
    | Nil 
    | Tag of string * attr list * t
    | Macro of string * t list
    | Seq of t list
    | Arg of int

  and attr = string * t

  type meta = 
    | Import of addr 
    | DefMacro of string * t

  type doc = 
    {metas : meta list;
     title : t;
     body : t}
end

module Sem = 
struct 
  type t = 
    | Text of string 
    | Transclude of addr 
    | Wikilink of t * addr 
    | Nil
    | Tag of string * attr list * t
    | Seq of t list

  and attr = string * t

  type doc = 
    {title : t; 
     body : t}
end
