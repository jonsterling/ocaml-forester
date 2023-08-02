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

let render_dvi_file ~name ~source =
  let tex_fp = tex_fp name in
  let dvi_fp = dvi_fp name in
  let pdf_fp = pdf_fp name in
  ensure_remove_file dvi_fp;
  ensure_remove_file pdf_fp;
  Proc.run "latex" ["-halt-on-error"; "-interaction=nonstopmode"; tex_fp]

let render_pdf_file ~name ~source =
  let tex_fp = tex_fp name in
  let pdf_fp = pdf_fp name in
  ensure_remove_file pdf_fp;
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
  let web_name = name ^ "-web" in
  let print_name = name ^ "-print" in
  begin
    if not @@ Sys.file_exists @@ web_name ^ ".svg" then
      begin
        write_tex_file ~name:web_name ~packages ~source;
        render_dvi_file ~name:web_name ~source;
        render_svg_file ~name:web_name ~source
      end
  end;
  begin
    if not @@ Sys.file_exists @@ print_name ^ ".pdf" then
      begin
        write_tex_file ~name:print_name ~packages:(packages @ ["newpxtext"; "newpxmath"]) ~source;
        render_pdf_file ~name:print_name ~source
      end
  end
