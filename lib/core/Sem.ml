open Base
open Prelude

type attr = string * string
[@@deriving show]

type node =
  | Text of string
  | Transclude of transclusion_mode * addr
  | Bibliography of t * transclusion_mode * addr Query.t
  | Link of {dest : string; title : t}
  | Tag of string * t
  | Math of math_mode * t
  | EmbedTeX of {packages : string list; source : t}
  | Block of t * t
[@@deriving show]

and t = node list

and env = t Env.t
[@@deriving show]

and clo = Clo of env * string list * Syn.t
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
   addr : addr;
   metas : (string * t) list;
   tags: addr list;
   body : t}
[@@deriving show]

module Doc = 
struct 
  let peek_title (doc : doc) =
    match doc.title with
    | Some (Text txt :: _) -> Some txt
    | _ -> None

  let sort =
    let by_date = Fun.flip @@ Compare.under (fun x -> x.date) @@ Compare.option Date.compare in
    let by_title = Compare.under peek_title @@ Compare.option String.compare in
    let by_addr = Compare.under (fun x -> x.addr) String.compare in
    List.sort @@ Compare.cascade by_date @@ Compare.cascade by_title by_addr
end
