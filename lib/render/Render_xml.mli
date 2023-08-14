open Core
open Bwd

type printer = Xmlm.output -> unit

val render_doc_page : trail:int bwd option -> Sem.doc -> printer
