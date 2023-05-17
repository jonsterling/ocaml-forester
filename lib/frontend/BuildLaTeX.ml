open Prelude.Shell
open Render

let tex_fp name =
  Format.sprintf "%s.tex" name

let dvi_fp name =
  Format.sprintf "%s.dvi" name

let pdf_fp name =
  Format.sprintf "%s.pdf" name

let write_tex_file ~name ~packages ~source =
  let tex_ch = open_out @@ tex_fp name in
  Fun.protect ~finally:(fun _ -> close_out tex_ch) @@ fun _ ->
  LaTeXTemplate.write tex_ch ~source ~packages

let render_dvi_and_pdf_file ~name ~source =
  let tex_fp = tex_fp name in
  let dvi_fp = dvi_fp name in
  let pdf_fp = pdf_fp name in
  ensure_remove_file dvi_fp;
  ensure_remove_file pdf_fp;
  Proc.run "latex" ["-halt-on-error"; "-interaction=nonstopmode"; tex_fp];
  Proc.run "pdflatex" ["-halt-on-error"; "-interaction=nonstopmode"; tex_fp]

let render_svg_file ~name ~source =
  let dvi_fp = dvi_fp name in
  Proc.run "dvisvgm"
    ["--exact";
     "--clipjoin";
     "--font-format=woff";
     "--bbox=papersize";
     "--zoom=1.5";
     dvi_fp;
     "--output=%f"];
  ensure_remove_file dvi_fp

let build_latex ~name ~packages ~source =
  let svg_fp = Format.sprintf "%s.svg" name in
  if not @@ Sys.file_exists svg_fp then
    begin
      write_tex_file ~name ~packages ~source;
      render_dvi_and_pdf_file ~name ~source;
      render_svg_file ~name ~source;
      Format.printf "Rendered %s@." svg_fp;
    end
