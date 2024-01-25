module type S =
sig
  val enqueue : name:string -> preamble:string -> source:string -> unit
  val process : env:_ Build_latex.env -> max_fibers:int -> ignore_tex_cache : bool -> Eio.Fs.dir_ty Eio.Path.t list
end

module Make () : S =
struct
  let svg_queue : (string, string * string) Hashtbl.t = Hashtbl.create 100

  let enqueue ~name ~preamble ~source =
    if not @@ Hashtbl.mem svg_queue name then
      Hashtbl.add svg_queue name (preamble, source)

  let process ~env ~max_fibers ~ignore_tex_cache  : Eio.Fs.dir_ty Eio.Path.t list =
    let build (name, (preamble, source)) =
      Build_latex.build_latex ~ignore_tex_cache ~env ~name ~source ~preamble
    in
    Hashtbl.to_seq svg_queue
    |> List.of_seq
    |> Eio.Fiber.List.map ~max_fibers build
    |> List.concat
end
