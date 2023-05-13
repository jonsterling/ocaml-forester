%{
  open Types
  
  module Frontlet = 
  struct
    open Code
    
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
      {fm with decls = Import addr :: fm.decls}

    let export addr fm = 
      {fm with decls = Export addr :: fm.decls}

    let author addr fm = 
      {fm with authors = addr :: fm.authors}
      
    let tag addr fm = 
      {fm with tags = addr :: fm.tags}
      
    let def (name, xs, body) fm = 
      {fm with decls = Def ([name], (xs, body)) :: fm.decls}
      
    let meta (key, bdy) fm = 
      {fm with metas = (key, bdy) :: fm.metas}
      
    let tex_package pkg fm = 
      {fm with tex_packages = pkg :: fm.tex_packages}

    let fold frontlets = 
      let open Code in
      let init = {title = None; taxon = None; date = None; decls = []; authors = []; tags = []; metas = []; tex_packages = []} in
      List.fold_right Fun.id frontlets init
  end
  
  let full_transclude x = Code.Transclude (Full, x)
  let splice_transclude x = Code.Transclude (Spliced, x)
  let collapse_transclude x = Code.Transclude (Collapsed, x)
%}

%token <string> TEXT IDENT
%token TRANSCLUDE TRANSCLUDE_STAR TRANSCLUDE_AT
%token TITLE IMPORT EXPORT DEF LET TEX TAXON AUTHOR TEX_PACKAGE TAG DATE BLOCK META
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token EOF

%type <Code.frontmatter -> Code.frontmatter> frontlet
%type <Code.t> expr
%start <Code.doc> main

%%

let braces(p) == delimited(LBRACE, p, RBRACE)
let squares(p) == delimited(LSQUARE, p, RSQUARE)
let parens(p) == delimited(LPAREN, p, RPAREN)
let binder == list(squares(TEXT))

let node :=
| ~ = braces(expr); <Code.braces>
| ~ = squares(expr); <Code.squares>
| ~ = parens(expr); <Code.parens> 
| ~ = delimited(HASH_LBRACE, expr, RBRACE); <Code.inline_math>
| ~ = delimited(HASH_HASH_LBRACE, expr, RBRACE); <Code.display_math>
| TRANSCLUDE; ~ = txt_arg; <full_transclude>
| TRANSCLUDE_STAR; ~ = txt_arg; <collapse_transclude>
| TRANSCLUDE_AT; ~ = txt_arg; <splice_transclude>
| LET; (~,~,~) = fun_spec; <Code.Let>
| TEX; ~ = arg; <Code.EmbedTeX>
| BLOCK; x = arg; y = arg; <Code.Block>
| ~ = IDENT; <Code.Ident>
| ~ = TEXT; <Code.Text>

let expr == list(node)
let arg == braces(expr)
let txt_arg == braces(TEXT)
let fun_spec == ~ = IDENT; ~ = binder; ~ = arg; <>

let frontlet := 
| TITLE; ~ = arg; <Frontlet.title>
| TAXON; ~ = txt_arg; <Frontlet.taxon>
| IMPORT; ~ = txt_arg; <Frontlet.import>
| EXPORT; ~ = txt_arg; <Frontlet.export>
| AUTHOR; ~ = txt_arg; <Frontlet.author>
| DATE; ~ = txt_arg; <Frontlet.date>
| DEF; ~ = fun_spec; <Frontlet.def>
| TAG; ~ = txt_arg; <Frontlet.tag>
| META; ~ = txt_arg; ~ = arg; <Frontlet.meta>
| TEX_PACKAGE; ~ = txt_arg; <Frontlet.tex_package>

let frontmatter == ~ = list(frontlet); <Frontlet.fold>
let main :=  ~ = frontmatter; ~ = expr; EOF; <> 
