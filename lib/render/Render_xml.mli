open Core
open Bwd

type printer = Xmlm.output -> unit

val render_doc_page : base_url:string option -> trail:int bwd option -> Sem.doc -> printer
