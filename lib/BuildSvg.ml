open Shell

let tex_fp name = 
  Format.sprintf "%s.tex" name

let dvi_fp name = 
  Format.sprintf "%s.dvi" name

let write_tex_file ~name ~source =
  let tex_ch = open_out @@ tex_fp name in
  Fun.protect ~finally:(fun _ -> close_out tex_ch) @@ fun _ ->
  LaTeXTemplate.write tex_ch source

let render_dvi_file ~name ~source =
  let tex_fp = tex_fp name in
  ensure_remove_file @@ dvi_fp name;
  Proc.run "latex" [tex_fp]

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

let build_svg ~name ~source =
  let dvi_fp = Format.sprintf "%s.dvi" name in
  let svg_fp = Format.sprintf "%s.svg" name in

  begin
    within_ensured_dir "build" @@ fun _ ->
    if not @@ Sys.file_exists svg_fp then 
      begin
        write_tex_file ~name ~source;
        render_dvi_file ~name ~source;
        render_svg_file ~name ~source
      end
  end
