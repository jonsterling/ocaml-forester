module T = Domainslib.Task

module type S =
sig
  val enqueue : name:string -> packages:string list -> source:string -> unit
  val process : env:_ BuildLaTeX.env -> unit
end

module Make (I : sig val max_fibers : int val ignore_tex_cache : bool end) : S =
struct
  let svg_queue : (string, string list * string) Hashtbl.t = Hashtbl.create 100

  let enqueue ~name ~packages ~source =
    if not @@ Hashtbl.mem svg_queue name then
      Hashtbl.add svg_queue name (packages, source)

  let process ~env : unit =
    Hashtbl.to_seq svg_queue |> List.of_seq |>
    Eio.Fiber.List.iter ~max_fibers:I.max_fibers @@ fun (name, (packages, source)) ->
    BuildLaTeX.build_latex ~ignore_tex_cache:I.ignore_tex_cache ~env ~name ~source ~packages
end
