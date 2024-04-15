open Core

module type Handler =
sig
  val route : addr -> string
  val is_root : addr -> bool
  val backlinks : addr -> Sem.tree list
  val related : addr -> Sem.tree list
  val bibliography : addr -> Sem.tree list
  val parents : addr -> Sem.tree list
  val children : addr -> Sem.tree list
  val contributors : addr -> addr list
  val contributions : addr -> Sem.tree list
  val enqueue_latex : name:string -> preamble:string -> source:string -> unit
  val get_doc : addr -> Sem.tree option
  val run_query : Sem.t Query.t -> Sem.tree list
  val last_changed : addr -> Prelude.Date.t option
end

type _ Effect.t +=
  | Route : addr -> string Effect.t
  | Is_root : addr -> bool Effect.t
  | Backlinks : addr -> Sem.tree list Effect.t
  | Related : addr -> Sem.tree list Effect.t
  | Bibliography : addr -> Sem.tree list Effect.t
  | Parents : addr -> Sem.tree list Effect.t
  | Children : addr -> Sem.tree list Effect.t
  | Contributions : addr -> Sem.tree list Effect.t
  | Contributors : addr -> addr list Effect.t
  | Enqueue_latex : {name : string; preamble : string; source : string} -> unit Effect.t
  | Get_doc : addr -> Sem.tree option Effect.t
  | Run_query : Sem.t Query.t -> Sem.tree list Effect.t
  | Last_changed : addr -> Prelude.Date.t option Effect.t

module Perform : Handler =
struct
  let route addr = Effect.perform @@ Route addr
  let is_root addr = Effect.perform @@ Is_root addr
  let backlinks addr = Effect.perform @@ Backlinks addr
  let related addr = Effect.perform @@ Related addr
  let bibliography addr = Effect.perform @@ Bibliography addr
  let contributions addr = Effect.perform @@ Contributions addr
  let parents addr = Effect.perform @@ Parents addr
  let children addr = Effect.perform @@ Children addr
  let contributors addr = Effect.perform @@ Contributors addr
  let enqueue_latex ~name ~preamble ~source = Effect.perform @@ Enqueue_latex {name; preamble; source}
  let get_doc addr = Effect.perform @@ Get_doc addr
  let run_query query = Effect.perform @@ Run_query query
  let last_changed addr = Effect.perform @@ Last_changed addr
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
         | Enqueue_latex {name; preamble; source} ->
           resume @@ fun () -> H.enqueue_latex ~name ~preamble ~source
         | Get_doc addr ->
           resume @@ fun () -> H.get_doc addr
         | Run_query query ->
           resume @@ fun () -> H.run_query query
         | Last_changed addr ->
           resume @@ fun () -> H.last_changed addr
         | _ ->
           None}
end
