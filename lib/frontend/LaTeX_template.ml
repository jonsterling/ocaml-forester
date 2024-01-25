let write fmt ~preamble ~source =
  let newline () = Format.fprintf fmt "\n" in
  Format.fprintf fmt {|\RequirePackage{ifpdf}|};
  newline ();
  Format.fprintf fmt {|\ifpdf\documentclass[crop]{standalone}\else\documentclass[crop,dvisvgm]{standalone}\fi|};
  newline ();
  Format.fprintf fmt "%s" preamble;
  newline ();
  Format.fprintf fmt {|\begin{document}|};
  newline ();
  Format.fprintf fmt "%s" source;
  newline ();
  Format.fprintf fmt {|\end{document}|}
