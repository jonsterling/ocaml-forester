open Core

type target =
  | Xml
  | Rss

module type Handler =
sig
  val route : target -> addr -> string
  val source_path : addr -> string option
  val is_root : addr -> bool
  val backlinks : addr -> Sem.doc list
  val related : addr -> Sem.doc list
  val bibliography : addr -> Sem.doc list
  val parents : addr -> Sem.doc list
  val children : addr -> Sem.doc list
  val contributors : addr -> string list
  val contributions : addr -> Sem.doc list
  val enqueue_latex : name:string -> packages:string list -> source:string -> unit
  val get_doc : addr -> Sem.doc option
  val run_query : Sem.t Query.t -> Sem.doc list
end

type _ Effect.t +=
  | Route : target * addr -> string Effect.t
  | Abs_path : addr -> string option Effect.t
  | Is_root : addr -> bool Effect.t
  | Backlinks : addr -> Sem.doc list Effect.t
  | Related : addr -> Sem.doc list Effect.t
  | Bibliography : addr -> Sem.doc list Effect.t
  | Parents : addr -> Sem.doc list Effect.t
  | Children : addr -> Sem.doc list Effect.t
  | Contributions : addr -> Sem.doc list Effect.t
  | Contributors : addr -> string list Effect.t
  | Enqueue_latex : {name : string; packages : string list; source : string} -> unit Effect.t
  | Get_doc : addr -> Sem.doc option Effect.t
  | Run_query : Sem.t Query.t -> Sem.doc list Effect.t

module Perform : Handler =
struct
  let route target addr = Effect.perform @@ Route (target, addr)
  let source_path addr = Effect.perform @@ Abs_path addr
  let is_root addr = Effect.perform @@ Is_root addr
  let backlinks addr = Effect.perform @@ Backlinks addr
  let related addr = Effect.perform @@ Related addr
  let bibliography addr = Effect.perform @@ Bibliography addr
  let contributions addr = Effect.perform @@ Contributions addr
  let parents addr = Effect.perform @@ Parents addr
  let children addr = Effect.perform @@ Children addr
  let contributors addr = Effect.perform @@ Contributors addr
  let enqueue_latex ~name ~packages ~source = Effect.perform @@ Enqueue_latex {name; packages; source}
  let get_doc addr = Effect.perform @@ Get_doc addr
  let run_query query = Effect.perform @@ Run_query query
end

module Run (H : Handler) =
struct
  let run f =
    Effect.Deep.try_with f () @@
    {effc =
       fun (type a) (eff : a Effect.t) ->
         let resume x =
           Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
           Algaeff.Fun.Deep.finally k @@ fun () -> x ()
         in
         match eff with
         | Route (target, addr) ->
           resume @@ fun () -> H.route target addr
         | Abs_path addr ->
           resume @@ fun () -> H.source_path addr
         | Is_root addr ->
           resume @@ fun () -> H.is_root addr
         | Backlinks addr ->
           resume @@ fun () -> H.backlinks addr
         | Related addr ->
           resume @@ fun () -> H.related addr
         | Bibliography addr ->
           resume @@ fun () -> H.bibliography addr
         | Parents addr ->
           resume @@ fun () -> H.parents addr
         | Children addr ->
           resume @@ fun () -> H.children addr
         | Contributors addr ->
           resume @@ fun () -> H.contributors addr
         | Contributions addr ->
           resume @@ fun () -> H.contributions addr
         | Enqueue_latex {name; packages; source} ->
           resume @@ fun () -> H.enqueue_latex ~name ~packages ~source
         | Get_doc addr ->
           resume @@ fun () -> H.get_doc addr
         | Run_query query ->
           resume @@ fun () -> H.run_query query
         | _ ->
           None}
end
