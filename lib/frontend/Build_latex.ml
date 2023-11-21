open Eio.Std
open Prelude
open Render

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

let pdf_fp name =
  Format.sprintf "%s.pdf" name

let build_dir cwd =
  Eio.Path.(cwd/"build")

let tex_fname name = name ^ ".tex"
let dvi_fname name = name ^ ".dvi"
let pdf_fname name = name ^ ".pdf"


let write_tex_file ~env ~name ~packages ~source =
  let cwd = Eio.Stdenv.cwd env in
  let path = Eio.Path.(build_dir cwd / tex_fname name) in
  let create = `Or_truncate 0o644 in
  Eio.Path.with_open_out ~create path @@ fun sink ->
  Eio.Buf_write.with_flow sink @@ fun writer ->
  let fmt = Eio_util.formatter_of_writer writer in
  LaTeX_template.write fmt ~source ~packages

let render_dvi_file ~env ~name ~source =
  let cwd = build_dir @@ Eio.Stdenv.cwd env in
  Eio_util.ensure_remove_file Eio.Path.(cwd / dvi_fname name);
  Eio_util.run_process ~env ~cwd
    ["latex"; "-halt-on-error"; "-interaction=nonstopmode"; tex_fname name]

let render_pdf_file ~env ~name ~source =
  let cwd = build_dir @@ Eio.Stdenv.cwd env in
  Eio_util.ensure_remove_file Eio.Path.(cwd / pdf_fname name);
  Eio_util.run_process ~env ~cwd
    ["pdflatex"; "-halt-on-error"; "-interaction=nonstopmode"; tex_fname name]

let render_svg_file ~env ~name ~source =
  let cwd = build_dir @@ Eio.Stdenv.cwd env in
  let fname = dvi_fname name in
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


let build_latex ~env ~ignore_tex_cache ~name ~packages ~source : Eio.Fs.dir_ty Eio.Path.t list =
  let cwd = Eio.Stdenv.cwd env in
  let web_name = name ^ "-web" in
  let print_name = name ^ "-print" in
  let svg_path = Eio.Path.(build_dir cwd / (web_name ^ ".svg")) in
  let pdf_path = Eio.Path.(build_dir cwd / (print_name ^ ".pdf")) in

  let svg_task () =
    if ignore_tex_cache || not @@ Eio_util.file_exists svg_path then
      begin
        write_tex_file ~env ~name:web_name ~packages ~source;
        render_dvi_file ~env ~name:web_name ~source;
        render_svg_file ~env ~name:web_name ~source;
        Some svg_path
      end
    else
      None
  in
  let pdf_task () =
    if ignore_tex_cache || not @@ Eio_util.file_exists pdf_path then
      begin
        write_tex_file ~env ~name:print_name ~packages:(packages @ ["newpxtext"; "newpxmath"]) ~source;
        render_pdf_file ~env ~name:print_name ~source;
        Some pdf_path
      end
    else
      None
  in

  [svg_task; pdf_task] |> Eio.Fiber.List.filter_map @@ fun task ->
  task ()
