type addr = string
[@@deriving show]

type delim = Braces | Squares | Parens
[@@deriving show]

type math_mode = Inline | Display
[@@deriving show]

module Expr = 
struct 
  type node = 
    | Text of string 
    | Group of delim * t
    | Math of math_mode * t
    | Tag of string
    | Transclude of string
    | EmbedTeX of t
    | Let of string * string list * t
    | Block of t * t
  [@@deriving show]

  and t = node list
  [@@deriving show]

  type macro = string list * t
  [@@deriving show]

  type frontmatter = 
    {title : t option;
     taxon : string option;
     imports : addr list;
     authors : addr list;
     tags : addr list;
     date: Date.t option;
     macros : (string * macro) list}
  [@@deriving show]

  type doc = frontmatter * t

  let inline_math e = Math (Inline, e)
  let display_math e = Math (Display, e)
  let parens e = Group (Parens, e)
  let squares e = Group (Squares, e)
  let braces e = Group (Braces, e)

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
    | Math of math_mode * t
    | EmbedTeX of t
    | Group of delim * t
    | Block of t * t
  [@@deriving show]

  and t = node list

  let rec node_map_text (f : string -> string) : node -> node =
    function 
    | Text str -> Text (f str)
    | Link {addr; title} -> Link {title = map_text f title; addr}
    | Tag (tag, attrs, xs) -> Tag (tag, attrs, List.map (map_text f) xs)
    | Group (delim, x) -> Group (delim, map_text f x)
    | node -> node

  and map_text (f : string -> string) : t -> t =
    List.map @@ node_map_text f


  type doc = 
    {title : t;
     taxon : string option;
     authors : addr list;
     date: Date.t option;
     addr : addr;
     body : t}
  [@@deriving show]


  module Doc =
  struct
    type t = doc

    let compare_for_sorting = 
      let compare_addrs doc0 doc1 = 
        String.compare doc0.addr doc1.addr
      in
      let compare_titles doc0 doc1 = 
        match doc0.title, doc1.title with
        | Text txt0 :: _, Text txt1 :: _ -> 
          let c = String.compare txt0 txt1 in 
          if c = 0 then compare_addrs doc0 doc1 else c
        | Text _ :: _, _ -> -1
        | _ -> compare_addrs doc0 doc1
      in
      let compare_dates doc0 doc1 = 
        match doc0.date, doc1.date with 
        | Some date0, Some date1 ->
          let c = Date.compare date0 date1 in 
          if c = 0 then compare_titles doc0 doc1 else - c
        | Some date0, None -> -1
        | _ -> compare_titles doc0 doc1
      in 
      compare_dates
  end

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
