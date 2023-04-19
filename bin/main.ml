open Entmoot

open Lexing

let colnum pos =
  pos.pos_cnum - pos.pos_bol - 1

let pos_string pos =
  Format.sprintf "line %i, column %i" pos.pos_lnum (colnum pos + 1)

let parse s =
  let lexbuf = Lexing.from_string s in
  try Parser.main Lexer.token lexbuf
  with Parser.Error ->
    failwith @@ "Parse error at " ^ pos_string lexbuf.lex_curr_p

let () =
  Format.print_newline ();
  let forest = new Forest.forest in

  forest#plant_tree "basics" @@
  parse {|
    \def{mem}{u}{v}{#{\u \in \v}}
    \def{def-em}{x}{\i{\b{\x}}}
  |};

  forest#plant_tree "book" @@ 
  parse {| 
    <<jms-004F>>
    <<jms-0050>>
  |};

  forest#plant_tree "jms-004F" @@
  parse {|
    \title{preduploid}
    \p{foo}
  |};

  forest#plant_tree "jms-0050" @@
  parse {|
    \title{duploid}
    \import{basics}

    \p{A [preduploid|jms-004F] #{D} is called a \def-em{duploid} when it satisfies the following properties:}

    \ul{
      \li{the preduploid #{D} is univalent;}
      \li{every positive object \mem{P}{D} has an upshift;}
      \li{every negative object of \mem{N}{D} has a downshift.}
    }
  |};

  forest#render_trees;
