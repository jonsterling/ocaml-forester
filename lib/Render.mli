open Types

type printer = Xmlm.output -> unit

class type env = 
  object 
    method transclude : addr -> printer
  end
  
val render : env -> Sem.t -> printer
