open Base
open Prelude

module MethodTable = Map.Make (String)

type node =
  | Text of string
  | Transclude of transclusion_opts * addr
  | Query of transclusion_opts * t Query.t
  | Link of {dest : string; title : t option; modifier : [`Sentence_case] option}
  | Xml_tag of string * (string * t) list * t
  | Unresolved of string
  | Math of math_mode * t
  | Embed_tex of {packages : string list; source : t}
  | Img of {path : string}
  | Block of t * t
  | If_tex of t * t
  | Prim of Prim.t * t
  | Object of Symbol.t
[@@deriving show]

and transclusion_opts =
  {toc : bool;
   show_heading : bool;
   show_metadata : bool;
   title_override : t option;
   taxon_override : string option;
   expanded : bool;
   numbered : bool}
[@@deriving show]

and t = node Range.located list

and env = t Env.t
[@@deriving show]

type obj_method =
  {body : Syn.t;
   self : Symbol.t;
   super : Symbol.t;
   env : env}

type obj =
  {prototype : Symbol.t option;
   methods : obj_method MethodTable.t}

let is_whitespace node =
  match Range.(node.value) with
  | Text txt -> String.trim txt = ""
  | _ -> false

let strip_whitespace =
  List.filter @@ fun x -> not @@ is_whitespace x

let trim_whitespace xs =
  let rec trim_front xs =
    match xs with
    | x :: xs when is_whitespace x ->
      trim_front xs
    | xs -> xs
  and trim_back xs =
    List.rev @@ trim_front @@ List.rev xs
  in
  trim_back @@ trim_front xs


let sentence_case nodes =
  let map_head f =
    function
    | [] -> []
    | x :: xs -> f x :: xs
  in
  let map_located f node =
    Range.{node with value = f node.value}
  in
  nodes |> map_head @@ map_located @@ function
  | Text str -> Text (String_util.sentence_case str)
  | Link link -> Link {link with modifier = Some `Sentence_case}
  | node -> node


let apply_modifier =
  function
  | Some `Sentence_case -> sentence_case
  | None -> Fun.id

type tree =
  {title : t option;
   taxon : string option;
   authors : addr list;
   dates : Date.t list;
   addr : addr option;
   metas : (string * t) list;
   tags: string list;
   body : t;
   source_path : string option}
[@@deriving show]

(** Best-effort rendering of a nodes as a string, to use in text-only contexts.*)
let string_of_nodes =
  let rec render nodes =
    String.concat "" @@
    List.filter_map render_node nodes
  and render_node located =
    match Range.(located.value) with
    | Text s -> Some s
    | Link {title = Some title; _} -> Some (render title)
    | Link {title = None; dest; _} -> Some dest
    | Xml_tag (_, _, bdy) | Math (_, bdy) -> Some (render bdy)
    | Embed_tex {source; _} -> Some (render source)
    | If_tex (_, x) -> Some (render x)
    | Prim (_, x) -> Some (render x)
    | Transclude _ | Query _ | Block _ | Unresolved _ | Img _ | Object _ -> None
  in
  render

module Util =
struct
  let peek_title (tree : tree) =
    match tree.title with
    | Some ({value = Text txt; _} :: _) -> Some txt
    | _ -> None

  let peek_addr (tree : tree) =
    tree.addr 

  let sort =
    let by_date = Fun.flip @@ Compare.under (fun x -> List.nth_opt x.dates 0) @@ Compare.option Date.compare in
    let by_title = Compare.under peek_title @@ Compare.option String.compare in
    let by_addr = Compare.under (fun x -> x.addr) @@ Compare.option String.compare in
    List.sort @@ Compare.cascade by_date @@ Compare.cascade by_title by_addr
end

module Query =
struct
  let rec test query (doc : tree) =
    match query with
    | Query.Author [Range.{value = Text addr; _}] ->
      List.mem addr doc.authors
    | Query.Tag [{value = Text addr; _}] ->
      List.mem addr doc.tags
    | Query.Meta (key, value) ->
      List.mem (key, value) doc.metas
    | Query.Taxon [{value = Text taxon; _}] ->
      doc.taxon = Some taxon
    | Query.Or qs ->
      qs |> List.exists @@ fun q -> test q doc
    | Query.And qs ->
      qs |> List.for_all @@ fun q -> test q doc
    | Query.Not q ->
      not @@ test q doc
    | Query.True ->
      true
    | _ -> false

end
