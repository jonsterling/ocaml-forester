let write ch ~packages ~source =
  Printf.fprintf ch "\\documentclass[crop,dvisvgm]{standalone}\n";
  begin
    packages |> List.iter @@ fun pkg ->
    Printf.fprintf ch "\\usepackage{%s}\n" pkg
  end;
  Printf.fprintf ch "\\begin{document}\n";
  Printf.fprintf ch "%s" source;
  Printf.fprintf ch "\n\\end{document}\n"
