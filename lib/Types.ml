type addr = string

module Syn =
struct
  type attr = string * string

  type node = 
    | Text of string 
    | Transclude of addr
    | Wikilink of t * addr
    | Tag of string * attr list * t list
    | Math of t
    | Title of t 
    | Import of addr 
    | DefMacro of string * string list * t
    | Let of string * string list * t * t
    | EmbedTeX of t

  and t = node list

end

module Sem = 
struct 
  type attr = string * string

  type node = 
    | Text of string 
    | Transclude of addr 
    | Wikilink of t * addr 
    | Tag of string * attr list * t list
    | Math of t
    | EmbedTeX of t

  and t = node list

  type doc = 
    {title : t; 
     body : t}
end

module Env = Map.Make (Symbol)

type clo = Clo of env * string list * Syn.t | Val of Sem.t
and env = clo Env.t
