open Base
open Prelude

type attr = string * string
[@@deriving show]


type node =
  | Text of string
  | Transclude of transclusion_opts * addr
  | Query of transclusion_opts * t Query.t
  | Link of {dest : string; title : t}
  | Tag of string * t
  | Math of math_mode * t
  | EmbedTeX of {packages : string list; source : t}
  | Block of t * t
[@@deriving show]

and transclusion_opts = 
  {toc : bool;
   show_heading : bool;
   show_metadata : bool;
   title_override : t option;
   expanded : bool; 
   numbered : bool}
[@@deriving show]

and t = node list

and env = t Env.t
[@@deriving show]

let sentence_case =
  function 
  | Text str :: xs -> Text (StringUtil.sentence_case str) :: xs
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

module Doc = 
struct 
  let peek_title (doc : doc) =
    match doc.title with
    | Some (Text txt :: _) -> Some txt
    | _ -> None

  (** Best-effort rendering of a doc title as a string, to use in text-only
      contexts.

      Returns [None] when the document does not have a title. Returns [Some s]
      otherwise, where [s] is a rendering of the title. Elements that could not
      be rendered are dropped. Consequently, when the title could not be
      rendered at all, [s] is the empty string. *)
  let title_as_string (doc : doc) : string option =
    let rec render nodes =
      String.concat "" (List.filter_map render_node nodes)
    and render_node = function
      | Text s -> Some s
      | Link {title; _} -> Some (render title)
      | Tag (_, bdy) | Math (_, bdy) -> Some (render bdy)
      | EmbedTeX {source; _} -> Some (render source)
      | Transclude _ | Query _ | Block _ -> None
    in
    Option.map (fun title -> StringUtil.sentence_case (render title)) doc.title

  let sort =
    let by_date = Fun.flip @@ Compare.under (fun x -> x.date) @@ Compare.option Date.compare in
    let by_title = Compare.under peek_title @@ Compare.option String.compare in
    let by_addr = Compare.under (fun x -> x.addr) @@ Compare.option String.compare in
    List.sort @@ Compare.cascade by_date @@ Compare.cascade by_title by_addr
end
