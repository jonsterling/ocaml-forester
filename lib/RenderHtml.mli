open Types

type printer = Xmlm.output -> unit

class type env =
  object
    method route : addr -> string
    method get_doc : addr -> Sem.doc
    method enqueue_svg : name:string -> source:string -> unit
  end

val render : env -> addr -> Sem.t -> printer
val render_doc : env -> addr -> Sem.doc -> printer
val render_doc_page : env -> addr -> Sem.doc -> printer
