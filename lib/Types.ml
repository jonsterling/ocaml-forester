type addr = string
[@@deriving show]

type delim = Braces | Squares | Parens
[@@deriving show]


module Expr = 
struct 
  type node = 
    | Text of string 
    | Group of delim * t
    | Math of t
    | Tag of string
    | Transclude of string
    | TeX of t
    | Let of string * string list * t
  [@@deriving show]

  and t = node list
  [@@deriving show]

  type macro = string list * t
  [@@deriving show]

  type frontmatter = 
    {title : t option;
     taxon : string option;
     imports : addr list;
     macros : (string * macro) list}
  [@@deriving show]

  type doc = frontmatter * t
end

module Sem = 
struct 
  type attr = string * string
  [@@deriving show]

  type node = 
    | Text of string 
    | Transclude of addr 
    | Link of {addr : addr; title : t}
    | Tag of string * attr list * t list
    | Math of t
    | EmbedTeX of t
    | Group of delim * t
  [@@deriving show]

  and t = node list

  type doc = 
    {title : t;
     taxon : string option;
     body : t}

  let rec node_map_text (f : string -> string) : node -> node =
    function 
    | Text str -> Text (f str)
    | Link {addr; title} -> Link {title = map_text f title; addr}
    | Tag (tag, attrs, xs) -> Tag (tag, attrs, List.map (map_text f) xs)
    | Group (delim, x) -> Group (delim, map_text f x)
    | node -> node

  and map_text (f : string -> string) : t -> t =
    List.map @@ node_map_text f


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

type clo = Clo of env * string list * Expr.t | Val of Sem.t
[@@deriving show]

and env = clo Env.t
[@@deriving show]
