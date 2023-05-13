open Types 

val expand : Term.t Env.t -> Code.t -> Term.t
val expand_macro : Term.t Env.t -> Code.macro -> Term.t
