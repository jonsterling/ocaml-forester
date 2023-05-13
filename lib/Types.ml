open struct module Y = Yuujinchou end

type addr = string
[@@deriving show]

type delim = Braces | Squares | Parens
[@@deriving show]

type math_mode = Inline | Display
[@@deriving show]

type transclusion_mode = Full | Collapsed | Spliced
[@@deriving show]

module Trie = 
struct 
  include Y.Trie
  let pp_path =
    let pp_sep fmt () = Format.pp_print_string fmt "." in
    Format.pp_print_list ~pp_sep Format.pp_print_string
end


module Code = 
struct 
  type node = 
    | Text of string 
    | Group of delim * t
    | Math of math_mode * t
    | Ident of string
    | Transclude of transclusion_mode * string
    | EmbedTeX of t
    | Let of string * string list * t
    | Block of t * t
  [@@deriving show]

  and t = node list
  [@@deriving show]

  type macro = string list * t
  [@@deriving show]

  type decl = 
    | Import of addr
    | Export of addr
    | Def of Trie.path * macro
  [@@deriving show]

  type frontmatter = 
    {title : t option;
     taxon : string option;
     authors : addr list;
     tags : addr list;
     date: Date.t option;
     metas : (string * t) list;
     decls : decl list}
  [@@deriving show]

  type doc = frontmatter * t

  let inline_math e = Math (Inline, e)
  let display_math e = Math (Display, e)
  let parens e = Group (Parens, e)
  let squares e = Group (Squares, e)
  let braces e = Group (Braces, e)
end

module Term = 
struct 
  type node = 
    | Text of string 
    | Group of delim * t
    | Math of math_mode * t
    | Tag of string
    | Link of {dest : string; title : t}
    | Transclude of transclusion_mode * string
    | EmbedTeX of t
    | Block of t * t
    | Lam of string list * t
    | Var of string
  [@@deriving show]

  and t = node list
  [@@deriving show]

  type macro = string list * t
  [@@deriving show]
end


module Env =
struct 
  include Map.Make (String)
  let pp (pp_el : Format.formatter -> 'a -> unit) : Format.formatter -> 'a t -> unit = 
    fun fmt map ->
    Format.fprintf fmt "@[{";
    begin 
      map |> iter @@ fun k v ->
      Format.fprintf fmt "@[%s ~> %a@;@]" k pp_el v
    end;
    Format.fprintf fmt "}@]"
end


module Sem = 
struct 
  type attr = string * string
  [@@deriving show]

  type node = 
    | Text of string 
    | Transclude of transclusion_mode * addr 
    | Link of {dest : string; title : t}
    | Tag of string * attr list * t list
    | Math of math_mode * t
    | EmbedTeX of t
    | Group of delim * t
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
     metas : (string * t) list;
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
