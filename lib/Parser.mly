%{
  open Types
  
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

    let date str =
      function
      | ({date = None; _} as fm) ->
        {fm with date = Some (Date.parse str)}
      | _ -> 
        failwith "Cannot set title twice"
    
    let import addr fm = 
      {fm with imports = addr :: fm.imports}

    let author addr fm = 
      {fm with authors = addr :: fm.authors}
      
    let tag addr fm = 
      {fm with tags = addr :: fm.tags}
      
    let def (name, xs, body) fm = 
      let macro = name, (xs, body) in 
      {fm with macros = macro :: fm.macros}
      
    let fold frontlets = 
      let open Expr in
      let init = {title = None; taxon = None; date = None; imports = []; macros = []; authors = []; tags = []} in
      List.fold_right Fun.id frontlets init
  end
%}

%token <string> TEXT FUN
%token TITLE IMPORT DEF LET TEX TRANSCLUDE TAXON AUTHOR TAG DATE BLOCK
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token EOF

%type <Expr.frontmatter -> Expr.frontmatter> frontlet
%type <Expr.t> expr
%start <Expr.doc> main

%%

let braces(p) == delimited(LBRACE, p, RBRACE)
let squares(p) == delimited(LSQUARE, p, RSQUARE)
let parens(p) == delimited(LPAREN, p, RPAREN)
let binder == list(squares(TEXT))

let node :=
| ~ = braces(expr); <Expr.braces>
| ~ = squares(expr); <Expr.squares>
| ~ = parens(expr); <Expr.parens> 
| ~ = delimited(HASH_LBRACE, expr, RBRACE); <Expr.inline_math>
| ~ = delimited(HASH_HASH_LBRACE, expr, RBRACE); <Expr.display_math>
| TRANSCLUDE; ~ = txt_arg; <Expr.Transclude>
| LET; (~,~,~) = fun_spec; <Expr.Let>
| TEX; ~ = arg; <Expr.EmbedTeX>
| BLOCK; x = arg; y = arg; <Expr.Block>
| ~ = FUN; <Expr.Tag>
| ~ = TEXT; <Expr.Text>

let expr == list(node)
let arg == braces(expr)
let txt_arg == braces(TEXT)
let fun_spec == ~ = FUN; ~ = binder; ~ = arg; <>

let frontlet := 
| TITLE; ~ = arg; <Frontlet.title>
| TAXON; ~ = txt_arg; <Frontlet.taxon>
| IMPORT; ~ = txt_arg; <Frontlet.import>
| AUTHOR; ~ = txt_arg; <Frontlet.author>
| DATE; ~ = txt_arg; <Frontlet.date>
| DEF; ~ = fun_spec; <Frontlet.def>
| TAG; ~ = txt_arg; <Frontlet.tag>

let frontmatter == ~ = list(frontlet); <Frontlet.fold>
let main :=  ~ = frontmatter; ~ = expr; EOF; <> 
