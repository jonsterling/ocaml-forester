open Eio.Std
open Forester_prelude
open Forester_render

type 'a env = 'a constraint 'a = <
    cwd : Eio.Fs.dir_ty Eio.Path.t;
    process_mgr : _ Eio.Process.mgr;
    stdout : _ Eio.Flow.sink;
    ..
  > as 'a


let tex_fp name =
  Format.sprintf "%s.tex" name

let dvi_fp name =
  Format.sprintf "%s.dvi" name

let build_dir cwd =
  Eio.Path.(cwd/"build")

let tex_fname name = name ^ ".tex"
let dvi_fname name = name ^ ".dvi"

let write_tex_file ~env ~name ~preamble ~source =
  let cwd = Eio.Stdenv.cwd env in
  let path = Eio.Path.(build_dir cwd / tex_fname name) in
  let create = `Or_truncate 0o644 in
  Eio.Path.with_open_out ~create path @@ fun sink ->
  Eio.Buf_write.with_flow sink @@ fun writer ->
  let fmt = Eio_util.formatter_of_writer writer in
  LaTeX_template.write fmt ~source ~preamble

let render_dvi_file ~env ~name ~source =
  let cwd = build_dir @@ Eio.Stdenv.cwd env in
  Eio_util.ensure_remove_file Eio.Path.(cwd / dvi_fname name);
  Eio.traceln "Building dvi for source: %s" name;
  Eio_util.run_process ~env ~cwd
    ["latex"; "-halt-on-error"; "-interaction=nonstopmode"; tex_fname name]

let render_svg_file ~env ~name ~source =
  let cwd = build_dir @@ Eio.Stdenv.cwd env in
  let fname = dvi_fname name in
  Eio.traceln "Building svg for source: %s" name;
  Eio_util.run_process ~env ~cwd
    ["dvisvgm";
     "--exact";
     "--clipjoin";
     "--font-format=woff";
     "--bbox=papersize";
     "--zoom=1.5";
     fname;
     "--output=%f"];
  Eio_util.ensure_remove_file Eio.Path.(cwd / fname)


let build_latex ~env ~ignore_tex_cache ~name ~preamble ~source : Eio.Fs.dir_ty Eio.Path.t list =
  let cwd = Eio.Stdenv.cwd env in
  let svg_path = Eio.Path.(build_dir cwd / (name ^ ".svg")) in

  write_tex_file ~env ~name ~preamble ~source;

  if ignore_tex_cache || not @@ Eio_util.file_exists svg_path then
    begin
      render_dvi_file ~env ~name ~source;
      render_svg_file ~env ~name ~source;
      [svg_path]
    end
  else
    []
