{
  exception SyntaxError of string
  let drop_sigil c str = 1 |> List.nth @@ String.split_on_char c str
  let macro str = Parser.MACRO (drop_sigil '\\' str)
  let illegal str = raise @@ SyntaxError ("Lexer - Illegal character: " ^ str)
  let illegal_verbatim str = raise @@ SyntaxError ("Verbatim Lexer - Illegal character: " ^ str)
  let text str = Parser.TEXT str
  
  let dbg str = Format.printf "%s\n" str; flush stdout
  
  type mode = [`Text | `Code]
  let mode_stk : mode Stack.t = Stack.create ()
  
  let _ = Stack.push `Text mode_stk
  
  let peek_mode () =
    let mode = Stack.pop mode_stk in 
    Stack.push mode mode_stk;
    mode 
  
  let only m tok lexbuf =
    if peek_mode () = m then tok else text (Lexing.lexeme lexbuf)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+  
let macro = '\\' (alpha) (alpha|digit|'_'|'-')*
let addr = (alpha) (alpha|digit|'_'|'-')* 
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let text = [^ '#' '\\' '{' '}' '[' ']' '|' '`']+
 
rule token =
  parse
  | '#' { Parser.MATH }
  | macro { macro (Lexing.lexeme lexbuf) }
  | "`{" { Stack.push `Code mode_stk; Parser.BEGIN_TEX }
  | "`}" { let _ = Stack.pop mode_stk in Parser.END_TEX }
  | '{' { Parser.LBRACE }
  | '}' { Parser.RBRACE }
  | '[' { only `Text Parser.LSQUARE lexbuf }
  | ']' { only `Text Parser.RSQUARE lexbuf }
  | '|' { only `Text Parser.PIPE lexbuf }
  | text { text (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | newline { token lexbuf } 
  | eof { Parser.EOF }
  | _ { illegal @@ Lexing.lexeme lexbuf }
