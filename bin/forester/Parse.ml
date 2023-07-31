open Forester
open Lexing

let colnum pos =
  pos.pos_cnum - pos.pos_bol - 1

let pp_pos fmt pos =
  Format.fprintf fmt "%s: line %i, column %i" pos.pos_fname pos.pos_lnum (colnum pos + 1)

let parse_channel filename ch =
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Parser.main Lexer.token lexbuf with
  | Parser.Error ->
    failwith @@ Format.asprintf "Parse error at %a" pp_pos lexbuf.lex_curr_p
  | Lexer.SyntaxError err ->
    failwith @@ Format.asprintf "Lexing error at %a : %s" pp_pos lexbuf.lex_curr_p err

let parse_file filename =
  let ch = open_in filename in
  Fun.protect ~finally:(fun _ -> close_in ch) @@ fun _ ->
  parse_channel filename ch
