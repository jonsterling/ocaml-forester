open Core
open Bwd

type printer = Xmlm.output -> unit

val render_doc_page : base_url:string -> Sem.doc -> printer
