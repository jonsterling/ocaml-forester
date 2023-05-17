open Core

type printer = Xmlm.output -> unit

val render_doc_page : addr -> Sem.doc -> printer
