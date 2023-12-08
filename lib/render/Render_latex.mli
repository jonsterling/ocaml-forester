open Core
val render : Sem.t -> Format.formatter -> unit
val render_tree_page : base_url:string option -> Sem.tree -> Format.formatter -> unit
