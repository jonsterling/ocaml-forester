open Types

type printer = Xmlm.output -> unit

type env = RenderEnv.t

type part = 
  | Top 
  | Frontmatter
  | Mainmatter 
  | Backmatter

type cfg = {part : part}

val render : cfg:cfg -> env -> Sem.t -> printer
val render_doc : cfg:cfg -> ?mode:transclusion_mode -> env -> Sem.doc -> printer
val render_doc_page : env -> addr -> Sem.doc -> printer
