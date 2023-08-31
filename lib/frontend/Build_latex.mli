open Eio.Std

type 'a env = 'a constraint 'a = <
    cwd : Eio.Fs.dir_ty Eio.Path.t;
    process_mgr : _ Eio.Process.mgr;
    stdout : _ Eio.Flow.sink;
    ..
  > as 'a

val build_latex
  : env:_ env
  -> ignore_tex_cache:bool
  -> name:string
  -> packages:string list
  -> source:string
  -> unit
