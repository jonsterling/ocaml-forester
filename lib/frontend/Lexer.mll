{
  exception SyntaxError of string
  let drop_sigil c str = 1 |> List.nth @@ String.split_on_char c str
  let ident str = Parser.IDENT (String.split_on_char '/' (drop_sigil '\\' str))
  let illegal str = raise @@ SyntaxError ("Lexer - Illegal character: [" ^ str ^ "].")

  let text str = Parser.TEXT str
  let dbg str = Format.printf "%s\n" str; flush stdout

  let verbatim = ref false

  let return_thunk lexbuf thunk =
    match !verbatim with
    | true -> text (Lexing.lexeme lexbuf)
    | false -> thunk ()

  let return lexbuf tok =
    return_thunk lexbuf @@ fun _ -> tok

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let ident = '\\' (alpha) (alpha|digit|'-'|'/')*
let addr = (alpha) (alpha|digit|'_'|'-')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let text = [^ '%' '#' '\\' '{' '}' '[' ']' '(' ')' '\n']+ newline?

rule token =
  parse
  | "%" { comment lexbuf }
  | "##{" { return lexbuf @@ Parser.HASH_HASH_LBRACE }
  | "#{" { return lexbuf @@ Parser.HASH_LBRACE }
  | "\\\\" { return lexbuf @@ Parser.IDENT [{|\|}] }
  | "\\," { return lexbuf @@ Parser.IDENT [{|,|}] }
  | "\\\"" { return lexbuf @@ Parser.IDENT [{|"|}] }
  | "\\'" { return lexbuf @@ Parser.IDENT [{|'|}] }
  | "\\`" { return lexbuf @@ Parser.IDENT [{|`|}] }
  | "\\_" { return lexbuf @@ Parser.IDENT [{|_|}] }
  | "\\;" { return lexbuf @@ Parser.IDENT [{|;|}] }
  | "\\#" { return lexbuf @@ Parser.IDENT [{|#|}] }
  | "\\{" { return lexbuf @@ Parser.IDENT [{|{|}] }
  | "\\}" { return lexbuf @@ Parser.IDENT [{|}|}] }
  | "\\[" { return lexbuf @@ Parser.IDENT [{|[|}] }
  | "\\]" { return lexbuf @@ Parser.IDENT [{|]|}] }
  | "\\startverb" { verbatim := true; token lexbuf }
  | "\\stopverb" { verbatim := false; token lexbuf }
  | "\\ " { return lexbuf @@ Parser.IDENT [{| |}] }
  | "\\title" { return lexbuf @@ Parser.TITLE }
  | "\\taxon" { return lexbuf @@ Parser.TAXON }
  | "\\author" { return lexbuf @@ Parser.AUTHOR }
  | "\\scope" { return lexbuf @@ Parser.SCOPE }
  | "\\put" { return lexbuf @@ Parser.PUT }
  | "\\put?" { return lexbuf @@ Parser.DEFAULT }
  | "\\get" { return lexbuf @@ Parser.GET }
  | "\\tag" { return lexbuf @@ Parser.TAG }
  | "\\date" { return lexbuf @@ Parser.DATE }
  | "\\import" { return lexbuf @@ Parser.IMPORT }
  | "\\export" { return lexbuf @@ Parser.EXPORT }
  | "\\namespace" { return lexbuf @@ Parser.NAMESPACE }
  | "\\open" { return lexbuf @@ Parser.OPEN }
  | "\\meta" { return lexbuf @@ Parser.META }
  | "\\def" { return lexbuf @@ Parser.DEF }
  | "\\alloc" { return lexbuf @@ Parser.ALLOC }
  | "\\let" { return lexbuf @@ Parser.LET }
  | "\\tex" { return lexbuf @@ Parser.TEX }
  | "\\block" { return lexbuf @@ Parser.BLOCK }
  | "\\texpackage" { return lexbuf @@ Parser.TEX_PACKAGE }
  | "\\transclude" { return lexbuf @@ Parser.TRANSCLUDE }
  | "\\query/and" {return lexbuf @@ Parser.QUERY_AND }
  | "\\query/or" {return lexbuf @@ Parser.QUERY_OR }
  | "\\query/author" {return lexbuf @@ Parser.QUERY_AUTHOR }
  | "\\query/tag" {return lexbuf @@ Parser.QUERY_TAG }
  | "\\query/taxon" {return lexbuf @@ Parser.QUERY_TAXON }
  | "\\query/meta" {return lexbuf @@ Parser.QUERY_META }
  | "\\query" { return lexbuf @@ Parser.QUERY_TREE }
  | "#" { return lexbuf @@ Parser.TEXT "#" }
  | ident { return lexbuf @@ ident (Lexing.lexeme lexbuf) }
  | '{' { return lexbuf @@ Parser.LBRACE }
  | '}' { return lexbuf @@ Parser.RBRACE }
  | '[' { return lexbuf @@ Parser.LSQUARE }
  | ']' { return lexbuf @@ Parser.RSQUARE }
  | '(' { return lexbuf @@ Parser.LPAREN }
  | ')' { return lexbuf @@ Parser.RPAREN }
  | text { text (Lexing.lexeme lexbuf) }
  | whitespace { return_thunk lexbuf @@ fun _ -> token lexbuf }
  | newline { Lexing.new_line lexbuf; return_thunk lexbuf @@ fun _ -> token lexbuf }
  | eof { Parser.EOF }
  | _ { illegal @@ Lexing.lexeme lexbuf }

and comment =
  parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }
