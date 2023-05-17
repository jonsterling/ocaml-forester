module T = Domainslib.Task

module type S =
sig
  val enqueue : name:string -> packages:string list -> source:string -> unit
  val process : unit -> unit
end

module Make (I : sig val size : int end) : S =
struct
  let svg_queue : (string, string list * string) Hashtbl.t = Hashtbl.create I.size

  let enqueue ~name ~packages ~source =
    if not @@ Hashtbl.mem svg_queue name then
      Hashtbl.add svg_queue name (packages, source)

  let process () : unit =
    let n = Hashtbl.length svg_queue in
    let tasks = Array.make n `Uninitialized in

    begin
      let i = ref 0 in
      svg_queue |> Hashtbl.iter @@ fun name (packages, source) ->
      tasks.(!i) <- `Task (name, packages, source);
      i := !i + 1
    end;

    Hashtbl.clear svg_queue;

    let worker i =
      match tasks.(i) with
      | `Task (name, packages, source) -> BuildLaTeX.build_latex ~name ~source ~packages
      | `Uninitialized -> failwith "Unexpected uninitialized task in LaTeX queue"
    in

    let pool = T.setup_pool ~num_domains:10 () in
    begin
      T.run pool @@ fun _ ->
      T.parallel_for pool ~start:0 ~finish:(n-1) ~body:worker
    end;
    T.teardown_pool pool;

    Hashtbl.clear svg_queue
end
