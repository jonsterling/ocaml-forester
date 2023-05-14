open Core

type printer = Xmlm.output -> unit

type part =
  | Top
  | Frontmatter
  | Mainmatter
  | Backmatter

type cfg = {part : part}

val render : cfg:cfg -> Sem.t -> printer
val render_doc : cfg:cfg -> ?mode:transclusion_mode -> Sem.doc -> printer

(* TODO: remove first argument *)
val render_doc_page : addr -> Sem.doc -> printer
