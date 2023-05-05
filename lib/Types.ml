type addr = string
[@@deriving show]

module Syn =
struct
  type attr = string * string
  [@@deriving show]

  type node = 
    | Text of string 
    | Transclude of addr
    | Wikilink of t * addr
    | Tag of string * attr list * t list
    | Math of t
    | Title of t 
    | Taxon of string
    | Import of addr 
    | DefMacro of string * string list * t
    | Let of string * string list * t * t
    | EmbedTeX of t
    | Group of t
  [@@deriving show]

  and t = node list

end

module Sem = 
struct 
  type attr = string * string
  [@@deriving show]

  type node = 
    | Text of string 
    | Transclude of addr 
    | Wikilink of t * addr 
    | Tag of string * attr list * t list
    | Math of t
    | EmbedTeX of t
    | Group of t
  [@@deriving show]

  and t = node list

  type doc = 
    {title : t; 
     body : t}
end

module Env = 
struct 
  include Map.Make (Symbol)
  let pp (pp_el : Format.formatter -> 'a -> unit) : Format.formatter -> 'a t -> unit = 
    fun fmt map ->
    Format.fprintf fmt "@[{";
    begin 
      map |> iter @@ fun k v ->
      Format.fprintf fmt "@[%a ~> %a@;@]" Symbol.pp k pp_el v
    end;
    Format.fprintf fmt "}@]"
end

type clo = Clo of env * string list * Syn.t | Val of Sem.t
[@@deriving show]

and env = clo Env.t
[@@deriving show]
