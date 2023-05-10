open Types

class type t = 
  object
    method route : addr -> string
    method get_doc : addr -> Sem.doc option
    method get_backlinks : addr -> Sem.doc list 
    method get_links : addr -> Sem.doc list 
    method get_parents : addr -> Sem.doc list
    method enqueue_svg : name:string -> source:string -> unit
  end