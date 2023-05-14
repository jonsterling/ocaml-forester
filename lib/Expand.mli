open Types 

val expand : Code.frontmatter -> Term.t Env.t -> Code.t -> Term.t
val expand_lambda : Code.frontmatter -> Term.t Env.t -> Code.macro -> Term.t
