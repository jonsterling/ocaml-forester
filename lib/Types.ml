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
     taxon : string option;
     body : t}

  let rec node_map_text (f : string -> string) : node -> node =
    function 
    | Text str -> Text (f str)
    | Wikilink (x, addr) -> Wikilink (map_text f x, addr) 
    | Tag (tag, attrs, xs) -> Tag (tag, attrs, List.map (map_text f) xs)
    | Group x -> Group (map_text f x)
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

type clo = Clo of env * string list * Syn.t | Val of Sem.t
[@@deriving show]

and env = clo Env.t
[@@deriving show]
