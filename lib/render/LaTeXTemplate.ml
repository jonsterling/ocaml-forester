let write ch ~packages ~source =
  let newline() = Printf.fprintf ch "\n" in
  Printf.fprintf ch {|\RequirePackage{ifpdf}|};
  newline ();
  Printf.fprintf ch {|\ifpdf\documentclass[crop]{standalone}\else\documentclass[crop,dvisvgm]{standalone}\fi|};
  newline ();
  begin
    packages |> List.iter @@ fun pkg ->
    Printf.fprintf ch {|\usepackage{%s}|} pkg;
    newline ();
  end;
  Printf.fprintf ch {|\begin{document}|};
  newline ();
  Printf.fprintf ch "%s" source;
  newline ();
  Printf.fprintf ch {|\end{document}|}
