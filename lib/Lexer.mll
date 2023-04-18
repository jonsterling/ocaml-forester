{
  exception SyntaxError of string
  let drop_sigil c str = 1 |> List.nth @@ String.split_on_char c str
  let bvar str = Parser.BVAR (int_of_string (drop_sigil '#' str))
  let macro str = Parser.MACRO (drop_sigil '\\' str)
  let tag str = Parser.TAG (drop_sigil '@' str)
  let illegal str = raise @@ SyntaxError ("Lexer - Illegal character: " ^ str)
  let text str = Parser.TEXT str
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+  
let macro = '\\' (alpha) (alpha|digit|'_')*
let tag = '@' (alpha) (alpha|digit|'_')*
let addr = (alpha) (alpha|digit|'_'|'-')* 
let whitespace = [' ' '\t']*
let newline = '\r' | '\n' | "\r\n"
let text = [^ '#' '\\' '@' '{' '}' '[' ']' '<' '>' '|']+
 
rule token =
  parse
  | whitespace { token lexbuf }
  | '#' int { bvar (Lexing.lexeme lexbuf) }
  | macro { macro (Lexing.lexeme lexbuf) }
  | tag { tag (Lexing.lexeme lexbuf) }
  | '{' { Parser.LBRACE }
  | '}' { Parser.RBRACE }
  | '[' { Parser.LSQUARE }
  | ']' { Parser.RSQUARE }
  | '|' { Parser.PIPE }
  | "<<" { Parser.LLANGLE }
  | ">>" { Parser.RRANGLE }
  | text { text (Lexing.lexeme lexbuf) }
  | newline { token lexbuf } 
  | eof { Parser.EOF }
  | _ { illegal @@ Lexing.lexeme lexbuf }
