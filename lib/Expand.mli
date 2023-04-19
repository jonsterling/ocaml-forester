open Types

type globals = string -> clo option 

val expand : globals -> env -> Syn.t -> Sem.t
