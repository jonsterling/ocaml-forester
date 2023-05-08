%{
  open Types
  
  let empty_frontmatter = 
    Expr.{title = None; taxon = None; imports = []; macros = []; authors = []}
%}

%token <string> TEXT MACRO
%token TITLE IMPORT DEF LET TEX TRANSCLUDE TAXON AUTHOR
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token EOF

%type <Expr.frontmatter -> Expr.frontmatter> front
%type <Expr.t> expr
%start <Expr.doc> main

%%

node:
| expr = braces(expr)
  { Expr.Group (Braces, expr) }
| expr = squares(expr) 
  { Expr.Group (Squares, expr) }
| expr = parens(expr)
  { Expr.Group (Parens, expr) }
| HASH_LBRACE expr = expr RBRACE
  { Expr.Math (Inline, expr) }
| HASH_HASH_LBRACE expr = expr RBRACE
  { Expr.Math (Display, expr) }
| TRANSCLUDE addr = braces(TEXT)
  { Expr.Transclude addr }
| tag = MACRO 
  { Expr.Tag tag }
| text = TEXT
  { Expr.Text text }
| LET name = MACRO xs = list(squares(TEXT)) body = braces(expr)
  { Expr.Let (name, xs, body) }
| TEX expr = braces(expr)
  { Expr.EmbedTeX expr }

expr:
| expr = list(node)
  { expr }

%inline
braces(p):
| LBRACE p = p RBRACE
  { p }
  
%inline
squares(p):
| LSQUARE p = p RSQUARE
  { p }

%inline
parens(p):
| LPAREN p = p RPAREN 
  { p }

front:
| TITLE expr = braces(expr)
  { function
    | ({title = None; _} as fm) ->
      {fm with title = Some expr}
    | _ -> 
      failwith "Cannot set title twice" }

| TAXON tax = braces(TEXT)
  { function
    | ({taxon = None; _} as fm) ->
      {fm with taxon = Some tax}
    | _ -> 
      failwith "Cannot set taxon twice" }

| IMPORT addr = braces(TEXT)
  { fun fm -> {fm with imports = addr :: fm.imports } }
  
| AUTHOR addr = braces(TEXT)
  { fun fm -> {fm with authors = addr :: fm.authors } }

| DEF name = MACRO xs = list(squares(TEXT)) body = braces(expr)
  { fun fm -> 
    let macro = name, (xs, body) in 
    {fm with macros = macro :: fm.macros} }

main: 
| fronts = list(front) expr = expr EOF
  { List.fold_right Fun.id fronts empty_frontmatter , expr }
