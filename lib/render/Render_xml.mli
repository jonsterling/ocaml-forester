open Core
open Bwd

type printer = Xmlm.output -> unit

val render_tree_page : base_url:string option -> trail:int bwd option -> Sem.tree -> printer
