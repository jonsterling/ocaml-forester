{
  exception SyntaxError of string
  let drop_sigil c str = 1 |> List.nth @@ String.split_on_char c str
  let ident str = Grammar.IDENT (drop_sigil '\\' str)
  let illegal str = raise @@ SyntaxError str

  let text str = Grammar.TEXT str
  let whitespace str = Grammar.WHITESPACE str
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
let ident = '\\' (alpha) (alpha|digit|'-'|'/'|'#')*
let addr = (alpha) (alpha|digit|'_'|'-')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let text = [^ ' ' '%' '#' '\\' '{' '}' '[' ']' '(' ')' '\r' '\n']+

rule token =
  parse
  | "\\%" { return lexbuf @@ Grammar.TEXT "%"}
  | "%" { comment lexbuf }
  | "##{" { return lexbuf @@ Grammar.HASH_HASH_LBRACE }
  | "#{" { return lexbuf @@ Grammar.HASH_LBRACE }
  | "\\\\" { return lexbuf @@ Grammar.IDENT {|\|} }
  | "\\," { return lexbuf @@ Grammar.IDENT {|,|} }
  | "\\\"" { return lexbuf @@ Grammar.IDENT {|"|} }
  | "\\'" { return lexbuf @@ Grammar.IDENT {|'|} }
  | "\\`" { return lexbuf @@ Grammar.IDENT {|`|} }
  | "\\_" { return lexbuf @@ Grammar.IDENT {|_|} }
  | "\\;" { return lexbuf @@ Grammar.IDENT {|;|} }
  | "\\#" { return lexbuf @@ Grammar.IDENT {|#|} }
  | "\\{" { return lexbuf @@ Grammar.IDENT {|{|} }
  | "\\}" { return lexbuf @@ Grammar.IDENT {|}|} }
  | "\\[" { return lexbuf @@ Grammar.IDENT {|[|} }
  | "\\]" { return lexbuf @@ Grammar.IDENT {|]|} }
  | "\\startverb" { verbatim := true; token lexbuf }
  | "\\stopverb" { verbatim := false; token lexbuf }
  | "\\ " { return lexbuf @@ Grammar.IDENT {| |} }
  | "\\title" { return lexbuf @@ Grammar.TITLE }
  | "\\taxon" { return lexbuf @@ Grammar.TAXON }
  | "\\author" { return lexbuf @@ Grammar.AUTHOR }
  | "\\contributor" { return lexbuf @@ Grammar.CONTRIBUTOR }
  | "\\scope" { return lexbuf @@ Grammar.SCOPE }
  | "\\put" { return lexbuf @@ Grammar.PUT }
  | "\\put?" { return lexbuf @@ Grammar.DEFAULT }
  | "\\get" { return lexbuf @@ Grammar.GET }
  | "\\tag" { return lexbuf @@ Grammar.TAG }
  | "\\ref" { return lexbuf @@ Grammar.REF }
  | "\\date" { return lexbuf @@ Grammar.DATE }
  | "\\import" { return lexbuf @@ Grammar.IMPORT }
  | "\\export" { return lexbuf @@ Grammar.EXPORT }
  | "\\namespace" { return lexbuf @@ Grammar.NAMESPACE }
  | "\\open" { return lexbuf @@ Grammar.OPEN }
  | "\\meta" { return lexbuf @@ Grammar.META }
  | "\\def" { return lexbuf @@ Grammar.DEF }
  | "\\alloc" { return lexbuf @@ Grammar.ALLOC }
  | "\\let" { return lexbuf @@ Grammar.LET }
  | "\\tex" { return lexbuf @@ Grammar.TEX }
  | "\\iftex" { return lexbuf @@ Grammar.IF_TEX }
  | "\\transclude" { return lexbuf @@ Grammar.TRANSCLUDE }
  | "\\subtree" { return lexbuf @@ Grammar.SUBTREE }
  | "\\query/and" {return lexbuf @@ Grammar.QUERY_AND }
  | "\\query/or" {return lexbuf @@ Grammar.QUERY_OR }
  | "\\query/author" {return lexbuf @@ Grammar.QUERY_AUTHOR }
  | "\\query/tag" {return lexbuf @@ Grammar.QUERY_TAG }
  | "\\query/taxon" {return lexbuf @@ Grammar.QUERY_TAXON }
  | "\\query/meta" {return lexbuf @@ Grammar.QUERY_META }
  | "\\query" { return lexbuf @@ Grammar.QUERY_TREE }
  | "\\xml" { return lexbuf @@ Grammar.XML_TAG }
  | "\\p" { return lexbuf @@ Grammar.PRIM `P }
  | "\\em" { return lexbuf @@ Grammar.PRIM `Em }
  | "\\strong" { return lexbuf @@ Grammar.PRIM `Strong }
  | "\\li" { return lexbuf @@ Grammar.PRIM `Li }
  | "\\ul" { return lexbuf @@ Grammar.PRIM `Ul }
  | "\\ol" { return lexbuf @@ Grammar.PRIM `Ol }
  | "\\code" { return lexbuf @@ Grammar.PRIM `Code }
  | "\\blockquote" { return lexbuf @@ Grammar.PRIM `Blockquote }
  | "\\pre" { return lexbuf @@ Grammar.PRIM `Pre }
  | "\\object" { return lexbuf @@ Grammar.OBJECT }
  | "\\patch" { return lexbuf @@ Grammar.PATCH }
  | "\\call" { return lexbuf @@ Grammar.CALL }
  | "#" { return lexbuf @@ Grammar.TEXT "#" }
  | ident { return lexbuf @@ ident (Lexing.lexeme lexbuf) }
  | '{' { return lexbuf @@ Grammar.LBRACE }
  | '}' { return lexbuf @@ Grammar.RBRACE }
  | '[' { return lexbuf @@ Grammar.LSQUARE }
  | ']' { return lexbuf @@ Grammar.RSQUARE }
  | '(' { return lexbuf @@ Grammar.LPAREN }
  | ')' { return lexbuf @@ Grammar.RPAREN }
  | text { text (Lexing.lexeme lexbuf) }
  | whitespace { whitespace (Lexing.lexeme lexbuf) }
  | newline { Lexing.new_line lexbuf; whitespace (Lexing.lexeme lexbuf) }
  | eof { Grammar.EOF }
  | _ { illegal @@ Lexing.lexeme lexbuf }

and comment =
  parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { Grammar.EOF }
  | _ { comment lexbuf }

