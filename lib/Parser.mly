%{
  open Types
  
  let empty_frontmatter = 
    Expr.{title = None; taxon = None; imports = []; macros = []; authors = []}
    
  module Frontlet = 
  struct
    open Expr
    
    let title title =
      function
      | ({title = None; _} as fm) ->
        {fm with title = Some title}
      | _ -> 
        failwith "Cannot set title twice"
        
    let taxon taxon =
      function
      | ({taxon = None; _} as fm) ->
        {fm with taxon = Some taxon}
      | _ -> 
        failwith "Cannot set taxon twice"
    
    let import addr fm = 
      {fm with imports = addr :: fm.imports}

    let author addr fm = 
      {fm with authors = addr :: fm.authors}
      
    let def (name, xs, body) fm = 
      let macro = name, (xs, body) in 
      {fm with macros = macro :: fm.macros}
      
    let fold frontlets = 
      List.fold_right Fun.id frontlets empty_frontmatter
  end
%}

%token <string> TEXT MACRO
%token TITLE IMPORT DEF LET TEX TRANSCLUDE TAXON AUTHOR
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token EOF

%type <Expr.frontmatter -> Expr.frontmatter> frontlet
%type <Expr.t> expr
%start <Expr.doc> main

%%

let braces(p) == LBRACE; ~ = p; RBRACE; <>
let squares(p) == LSQUARE; ~ = p; RSQUARE; <>
let parens(p) == LPAREN; ~ = p; RPAREN; <>

let node :=
| ~ = braces(expr); <Expr.braces>
| ~ = squares(expr); <Expr.squares>
| ~ = parens(expr); <Expr.parens> 
| HASH_LBRACE; ~ = expr; RBRACE; <Expr.display_math>
| HASH_HASH_LBRACE; ~ = expr; RBRACE; <Expr.display_math>
| TRANSCLUDE; ~ = braces(TEXT); <Expr.Transclude>
| ~ = MACRO; <Expr.Tag>
| ~ = TEXT; <Expr.Text>
| LET; ~ = MACRO; ~ = list(squares(TEXT)); ~ = braces(expr); <Expr.Let>
| TEX; ~ = braces(expr); <Expr.EmbedTeX>

let expr == ~ = list(node); <>

let frontlet := 
| TITLE; ~ = braces(expr); <Frontlet.title>
| TAXON; ~ = braces(TEXT); <Frontlet.taxon>
| IMPORT; ~ = braces(TEXT); <Frontlet.import>
| AUTHOR; ~ = braces(TEXT); <Frontlet.author>
| DEF; ~ = MACRO; ~ = list(squares(TEXT)); ~ = braces(expr); <Frontlet.def>

let frontmatter == ~ = list(frontlet); <Frontlet.fold>
let main :=  ~ = frontmatter; ~ = expr; EOF; <> 
