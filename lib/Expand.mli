open Types

type globals = Symbol.t -> clo option 

val expand_nodes : globals -> env -> Syn.t -> Sem.t
