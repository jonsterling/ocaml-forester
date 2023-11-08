open Base
open Prelude

type node =
  | Text of string
  | Transclude of transclusion_opts * addr
  | Query of transclusion_opts * t Query.t
  | Link of {dest : string; title : t option}
  | Xml_tag of string * (string * t) list * t
  | Unresolved of string
  | Math of math_mode * t
  | Embed_tex of {packages : string list; source : t}
  | Img of {path : string}
  | Block of t * t
  | If_tex of t * t
  | Prim of Prim.t * t
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

let rec sentence_case =
  function
  | Range.{value = Text str; loc} :: xs ->
    Range.{value = Text (String_util.sentence_case str); loc} :: xs
  | Range.{value = Link {title; dest}; loc} :: xs ->
    {Range.value = Link {title = Option.map sentence_case title; dest}; loc} :: xs
  | xs -> xs

type doc =
  {title : t option;
   taxon : string option;
   authors : addr list;
   date: Date.t option;
   addr : addr option;
   metas : (string * t) list;
   tags: string list;
   body : t}
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
    | Link {title = None; dest} -> Some dest
    | Xml_tag (_, _, bdy) | Math (_, bdy) -> Some (render bdy)
    | Embed_tex {source; _} -> Some (render source)
    | If_tex (_, x) -> Some (render x)
    | Prim (_, x) -> Some (render x)
    | Transclude _ | Query _ | Block _ | Unresolved _ | Img _ -> None
  in
  render

module Doc =
struct
  let peek_title (doc : doc) =
    match doc.title with
    | Some ({value = Text txt; _} :: _) -> Some txt
    | _ -> None

  let sort =
    let by_date = Fun.flip @@ Compare.under (fun x -> x.date) @@ Compare.option Date.compare in
    let by_title = Compare.under peek_title @@ Compare.option String.compare in
    let by_addr = Compare.under (fun x -> x.addr) @@ Compare.option String.compare in
    List.sort @@ Compare.cascade by_date @@ Compare.cascade by_title by_addr
end
