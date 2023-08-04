let write fmt ~packages ~source =
  let newline () = Format.fprintf fmt "\n" in
  Format.fprintf fmt {|\RequirePackage{ifpdf}|};
  newline ();
  Format.fprintf fmt {|\ifpdf\documentclass[crop]{standalone}\else\documentclass[crop,dvisvgm]{standalone}\fi|};
  newline ();
  begin
    packages |> List.iter @@ fun pkg ->
    Format.fprintf fmt {|\usepackage{%s}|} pkg;
    newline ();
  end;
  Format.fprintf fmt {|\begin{document}|};
  newline ();
  Format.fprintf fmt "%s" source;
  newline ();
  Format.fprintf fmt {|\end{document}|}
