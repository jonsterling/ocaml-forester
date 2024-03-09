{
  open Prelude
  exception SyntaxError of string
  let drop_sigil c str = 1 |> List.nth @@ String.split_on_char c str
  let verbatim_buff = Buffer.create 1000
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let ident = '\\' (alpha) (alpha|digit|'-'|'/'|'#')*
let addr = (alpha) (alpha|digit|'_'|'-')*
let wschar = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let text = [^ ' ' '%' '#' '\\' '{' '}' '[' ']' '(' ')' '\r' '\n']+

rule token =
  parse
  | "\\%" { Grammar.TEXT "%"}
  | "%" { comment lexbuf }
  | "##{" { Grammar.HASH_HASH_LBRACE }
  | "#{" { Grammar.HASH_LBRACE }
  | "\\\\" { Grammar.IDENT {|\|} }
  | "\\," { Grammar.IDENT {|,|} }
  | "\\\"" { Grammar.IDENT {|"|} }
  | "\\'" { Grammar.IDENT {|'|} }
  | "\\`" { Grammar.IDENT {|`|} }
  | "\\_" { Grammar.IDENT {|_|} }
  | "\\;" { Grammar.IDENT {|;|} }
  | "\\#" { Grammar.IDENT {|#|} }
  | "\\{" { Grammar.IDENT {|{|} }
  | "\\}" { Grammar.IDENT {|}|} }
  | "\\[" { Grammar.IDENT {|[|} }
  | "\\]" { Grammar.IDENT {|]|} }
  | "\\startverb" {
      Buffer.clear verbatim_buff;
      verbatim lexbuf;
      let text = String_util.trim_trailing_whitespace @@ String_util.trim_newlines @@ Buffer.contents verbatim_buff in
      Grammar.TEXT text
    }
  | "\\ " { Grammar.IDENT {| |} }
  | "\\title" { Grammar.TITLE }
  | "\\parent" { Grammar.PARENT }
  | "\\taxon" { Grammar.TAXON }
  | "\\author" { Grammar.AUTHOR }
  | "\\contributor" { Grammar.CONTRIBUTOR }
  | "\\scope" { Grammar.SCOPE }
  | "\\put" { Grammar.PUT }
  | "\\put?" { Grammar.DEFAULT }
  | "\\get" { Grammar.GET }
  | "\\tag" { Grammar.TAG }
  | "\\ref" { Grammar.REF }
  | "\\date" { Grammar.DATE }
  | "\\number" { Grammar.NUMBER }
  | "\\import" { Grammar.IMPORT }
  | "\\export" { Grammar.EXPORT }
  | "\\namespace" { Grammar.NAMESPACE }
  | "\\open" { Grammar.OPEN }
  | "\\meta" { Grammar.META }
  | "\\def" { Grammar.DEF }
  | "\\alloc" { Grammar.ALLOC }
  | "\\let" { Grammar.LET }
  | "\\tex" { Grammar.TEX }
  | "\\transclude" { Grammar.TRANSCLUDE }
  | "\\subtree" { Grammar.SUBTREE }
  | "\\query/and" {Grammar.QUERY_AND }
  | "\\query/or" {Grammar.QUERY_OR }
  | "\\query/not" {Grammar.QUERY_NOT }
  | "\\query/author" {Grammar.QUERY_AUTHOR }
  | "\\query/tag" {Grammar.QUERY_TAG }
  | "\\query/taxon" {Grammar.QUERY_TAXON }
  | "\\query/meta" {Grammar.QUERY_META }
  | "\\query" { Grammar.QUERY_TREE }
  | "\\xml" { Grammar.XML_TAG }
  | "\\p" { Grammar.PRIM `P }
  | "\\em" { Grammar.PRIM `Em }
  | "\\strong" { Grammar.PRIM `Strong }
  | "\\li" { Grammar.PRIM `Li }
  | "\\ul" { Grammar.PRIM `Ul }
  | "\\ol" { Grammar.PRIM `Ol }
  | "\\code" { Grammar.PRIM `Code }
  | "\\blockquote" { Grammar.PRIM `Blockquote }
  | "\\pre" { Grammar.PRIM `Pre }
  | "\\object" { Grammar.OBJECT }
  | "\\patch" { Grammar.PATCH }
  | "\\call" { Grammar.CALL }
  | "#" { Grammar.TEXT "#" }
  | ident { Grammar.IDENT (drop_sigil '\\' (Lexing.lexeme lexbuf)) }
  | '{' { Grammar.LBRACE }
  | '}' { Grammar.RBRACE }
  | '[' { Grammar.LSQUARE }
  | ']' { Grammar.RSQUARE }
  | '(' { Grammar.LPAREN }
  | ')' { Grammar.RPAREN }
  | text { Grammar.TEXT (Lexing.lexeme lexbuf) }
  | wschar+ { Grammar.WHITESPACE (Lexing.lexeme lexbuf) }
  | newline { Lexing.new_line lexbuf; Grammar.WHITESPACE (Lexing.lexeme lexbuf) }
  | eof { Grammar.EOF }
  | _ { raise @@ SyntaxError (Lexing.lexeme lexbuf) }

and comment =
  parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { Grammar.EOF }
  | _ { comment lexbuf }

and verbatim =
  parse
  | "\\stopverb" { () }
  | _ as c
    { Buffer.add_char verbatim_buff c;
      verbatim lexbuf }
