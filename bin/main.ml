open Forester
open Lexing

let colnum pos =
  pos.pos_cnum - pos.pos_bol - 1

let pos_string pos =
  Format.sprintf "%s: line %i, column %i" pos.pos_fname pos.pos_lnum (colnum pos + 1)

let parse_channel filename ch =
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Parser.main Lexer.token lexbuf
  with 
  | Parser.Error ->
    failwith @@ "Parse error at " ^ pos_string lexbuf.lex_curr_p
  | Lexer.SyntaxError err -> 
    failwith @@ "Lexing error at " ^ pos_string lexbuf.lex_curr_p ^ ": " ^ err

let parse_file filename = 
  let ch = open_in filename in 
  Fun.protect ~finally:(fun _ -> close_in ch) @@ fun _ -> 
  parse_channel filename ch 

let process_file forest filename =
  if Filename.check_suffix filename "tree" then 
    let addr = Filename.chop_extension @@ Filename.basename filename in
    Format.printf "Processing %s\n" addr;
    forest#plant_tree addr @@ 
    parse_file filename

let process_dir forest dir =
  Sys.readdir dir |> Array.iter @@ fun filename ->
  process_file forest @@ dir ^ "/" ^ filename

let () =
  Format.print_newline ();

  let forest = new Forest.forest in

  let usage_msg = "forester <dir> ..." in
  let input_dirs = ref [] in
  let anon_fun dir = input_dirs := dir :: !input_dirs  in
  let () = Arg.parse [] anon_fun usage_msg in
  begin 
    !input_dirs |> List.iter @@ fun dir -> 
    process_dir forest dir
  end;
  forest#render_trees
