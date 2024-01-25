module type S =
sig
  val enqueue : name:string -> preamble:string -> source:string -> unit
  val process : env:_ Build_latex.env -> max_fibers:int -> ignore_tex_cache : bool -> Eio.Fs.dir_ty Eio.Path.t list
end

module Make () : S
