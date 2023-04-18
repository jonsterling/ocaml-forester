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
    \def{p}{@p{#1}}
  |};

  forest#plant_tree "jms-0002" @@ 
  parse {|
  \title{adsfadf}
  \import{basics}

  \p{asdfasdf}

  \p{asdf}
  |};

  forest#render_trees;
  (* 
  let forest = new Forest.basic_forest in 

  forest#plant_tree "jms-0002" @@ 
  parse {|
    \import{jms-0001}
    \p{[\mymacro{\coolname} | jms-0001]}

    transclusion: <<jms-0003>>
  |};

  forest#plant_tree "jms-0001" @@ 
  parse {|
    \import{jms-0003}
    \p{this one has some backlinks}
    \defmacro\mymacro{Hello, #1}
  |};

  forest#plant_tree "jms-0003" @@ 
  parse {|
    \defmacro\coolname{Fred}
    \p{hello, \b{\coolname} \m{adf}}
    \p{asdfadsf}
    \p{asdfasdsss}
  |};

  forest#render_all Format.std_formatter *)
