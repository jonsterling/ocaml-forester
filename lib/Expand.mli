open Types

type globals = Symbol.t -> clo option 

val expand : globals -> env -> Syn.t -> Sem.t
