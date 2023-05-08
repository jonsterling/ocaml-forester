%{
  open Types
%}

%token <string> TEXT MACRO
%token TITLE IMPORT DEF LET TEX TRANSCLUDE TAXON
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
  { fun fm -> {fm with title = Some expr} }
| TAXON tax = braces(TEXT)
  { fun fm -> {fm with taxon = Some tax} }
| IMPORT addr = braces(TEXT)
  { fun fm -> {fm with imports = fm.imports @ [addr] }}
| DEF name = MACRO xs = list(squares(TEXT)) body = braces(expr)
  { fun fm -> 
    let macro = name, (xs, body) in 
    {fm with macros = fm.macros @ [macro]} }

main: 
| fronts = list(front) expr = expr EOF
  { let init = Expr.{title = None; taxon = None; imports = []; macros = []} in 
    List.fold_left (fun fm phi -> phi fm) init fronts, expr }
