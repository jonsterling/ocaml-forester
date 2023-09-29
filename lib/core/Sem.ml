open Base
open Prelude

type attr = string * string
[@@deriving show]

type node =
  | Text of string
  | Transclude of transclusion_opts * addr
  | Query of transclusion_opts * t Query.t
  | Link of {dest : string; title : t option}
  | Tag of string * attr list * t
  | Math of math_mode * t
  | Embed_tex of {packages : string list; source : t}
  | Block of t * t
  | If_tex of t * t
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

and t = node list

and env = t Env.t
[@@deriving show]

let sentence_case =
  function
  | Text str :: xs -> Text (String_util.sentence_case str) :: xs
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
  and
    render_node = function
    | Text s -> Some s
    | Link {title = Some title; _} -> Some (render title)
    | Link {title = None; dest} -> Some dest
    | Tag (_, _, bdy) | Math (_, bdy) -> Some (render bdy)
    | Embed_tex {source; _} -> Some (render source)
    | If_tex (_, x) -> Some (render x)
    | Transclude _ | Query _ | Block _ -> None
  in
  render

module Doc =
struct
  let peek_title (doc : doc) =
    match doc.title with
    | Some (Text txt :: _) -> Some txt
    | _ -> None

  let sort =
    let by_date = Fun.flip @@ Compare.under (fun x -> x.date) @@ Compare.option Date.compare in
    let by_title = Compare.under peek_title @@ Compare.option String.compare in
    let by_addr = Compare.under (fun x -> x.addr) @@ Compare.option String.compare in
    List.sort @@ Compare.cascade by_date @@ Compare.cascade by_title by_addr
end
