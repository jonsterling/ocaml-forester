open Forester
open Core
open Lexing

let parse_channel filename ch =
  Reporter.tracef "when parsing file `%s`" filename @@ fun () ->
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Grammar.main Lexer.token lexbuf with
  | Grammar.Error ->
    let loc = Asai.Range.of_lexbuf lexbuf in
    Reporter.fatalf ~loc Parse_error "failed to parse `%s`" (Lexing.lexeme lexbuf)
  | Lexer.SyntaxError token ->
    let loc = Asai.Range.of_lexbuf lexbuf in
    Reporter.fatalf ~loc Parse_error "unrecognized token `%s`" @@
    String.escaped token

let parse_file fp =
  let filename = Eio.Path.native_exn fp in
  let ch = open_in filename in
  Fun.protect ~finally:(fun _ -> close_in ch) @@ fun _ ->
  parse_channel filename ch

