open Core

module type Handler =
sig
  val route : addr -> string
  val abs_path : addr -> string option
  val is_root : addr -> bool
  val backlinks : addr -> Sem.doc list
  val related : addr -> Sem.doc list
  val bibliography : addr -> Sem.doc list
  val parents : addr -> Sem.doc list
  val contributors : addr -> string list
  val contributions : addr -> Sem.doc list
  val enqueue_latex : name:string -> packages:string list -> source:string -> unit
  val get_doc : addr -> Sem.doc option
  val run_query : Sem.t Query.t -> Sem.doc list
end

type _ Effect.t +=
  | Route : addr -> string Effect.t
  | AbsPath : addr -> string option Effect.t
  | IsRoot : addr -> bool Effect.t
  | Backlinks : addr -> Sem.doc list Effect.t
  | Related : addr -> Sem.doc list Effect.t
  | Bibliography : addr -> Sem.doc list Effect.t
  | Parents : addr -> Sem.doc list Effect.t
  | Contributions : addr -> Sem.doc list Effect.t
  | Contributors : addr -> string list Effect.t
  | EnqueueLaTeX : {name : string; packages : string list; source : string} -> unit Effect.t
  | GetDoc : addr -> Sem.doc option Effect.t
  | RunQuery : Sem.t Query.t -> Sem.doc list Effect.t

module Perform : Handler =
struct
  let route addr = Effect.perform @@ Route addr
  let abs_path addr = Effect.perform @@ AbsPath addr
  let is_root addr = Effect.perform @@ IsRoot addr
  let backlinks addr = Effect.perform @@ Backlinks addr
  let related addr = Effect.perform @@ Related addr
  let bibliography addr = Effect.perform @@ Bibliography addr
  let contributions addr = Effect.perform @@ Contributions addr
  let parents addr = Effect.perform @@ Parents addr
  let contributors addr = Effect.perform @@ Contributors addr
  let enqueue_latex ~name ~packages ~source = Effect.perform @@ EnqueueLaTeX {name; packages; source}
  let get_doc addr = Effect.perform @@ GetDoc addr
  let run_query query = Effect.perform @@ RunQuery query
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
         | Route addr ->
           resume @@ fun () -> H.route addr
         | AbsPath addr ->
           resume @@ fun () -> H.abs_path addr
         | IsRoot addr ->
           resume @@ fun () -> H.is_root addr
         | Backlinks addr ->
           resume @@ fun () -> H.backlinks addr
         | Related addr ->
           resume @@ fun () -> H.related addr
         | Bibliography addr ->
           resume @@ fun () -> H.bibliography addr
         | Parents addr ->
           resume @@ fun () -> H.parents addr
         | Contributors addr ->
           resume @@ fun () -> H.contributors addr
         | Contributions addr ->
           resume @@ fun () -> H.contributions addr
         | EnqueueLaTeX {name; packages; source} ->
           resume @@ fun () -> H.enqueue_latex ~name ~packages ~source
         | GetDoc addr ->
           resume @@ fun () -> H.get_doc addr
         | RunQuery query ->
           resume @@ fun () -> H.run_query query
         | _ ->
           None}
end
