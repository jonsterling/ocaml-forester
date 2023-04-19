open Types

type printer = Xmlm.output -> unit

class type env = 
  object 
    method route : addr -> string
    method transclude : addr -> printer
  end
  
val render : env -> Sem.t -> printer
val render_doc : env -> Sem.doc -> printer
val render_doc_page : env -> Sem.doc -> printer
