{
  exception SyntaxError of string
  let drop_sigil c str = 1 |> List.nth @@ String.split_on_char c str
  let macro str = Parser.MACRO (drop_sigil '\\' str)
  let illegal str = raise @@ SyntaxError ("Lexer - Illegal character: [" ^ str ^ "].")
  
  let text str = Parser.TEXT str
  let dbg str = Format.printf "%s\n" str; flush stdout
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let int = '-'? digit+  
let macro = '\\' (alpha) (alpha|digit|'_'|'-')*
let addr = (alpha) (alpha|digit|'_'|'-')* 
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let text = [^ '#' '\\' '{' '}' '[' ']' '(' ')' '`' '\n']+
 
rule token =
  parse
  | '#' { Parser.MATH }
  | "\\\\" { Parser.MACRO {|\|} }
  | "\\," { Parser.MACRO {|\,|} }
  | "\\;" { Parser.MACRO {|\;|} }
  | "\\title" { Parser.TITLE }
  | "\\taxon" { Parser.TAXON }
  | "\\import" { Parser.IMPORT }
  | "\\def" { Parser.DEF }
  | "\\let" { Parser.LET }
  | "\\tex" { Parser.TEX }
  | "\\transclude" { Parser.TRANSCLUDE }
  | macro { macro (Lexing.lexeme lexbuf) }
  | '{' { Parser.LBRACE }
  | '}' { Parser.RBRACE }
  | '[' { Parser.LSQUARE }
  | ']' { Parser.RSQUARE }
  | '(' { Parser.LPAREN }
  | ')' { Parser.RPAREN }
  | text { text (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf } 
  | eof { Parser.EOF }
  | _ { illegal @@ Lexing.lexeme lexbuf }
