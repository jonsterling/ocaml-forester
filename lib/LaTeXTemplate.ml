let before = 
  {| 
  \documentclass[crop,dvisvgm]{standalone}
  \usepackage{tikz, tikz-cd, mathtools, amssymb, stmaryrd}
  \begin{document}
  |}

let after = 
  {|
  \end{document}
  |}

let write ch source =
  Printf.fprintf ch "%s%s%s" before source after
