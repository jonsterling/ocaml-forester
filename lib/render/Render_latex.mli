open Core
val render : Sem.t -> Format.formatter -> unit
val render_doc_page : base_url:string option -> Sem.doc -> Format.formatter -> unit
