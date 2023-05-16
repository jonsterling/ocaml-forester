%{
  open Prelude
  open Core

  let full_transclude x = Code.Transclude (Full, x)
  let splice_transclude x = Code.Transclude (Spliced, x)
  let collapse_transclude x = Code.Transclude (Collapsed, x)

%}

%token <string> TEXT 
%token <string list> IDENT
%token TITLE IMPORT EXPORT DEF TAXON AUTHOR TEX_PACKAGE TAG DATE NAMESPACE LET TEX BLOCK META OPEN
%token TRANSCLUDE TRANSCLUDE_STAR TRANSCLUDE_AT SCOPE PUT GET DEFAULT ALLOC 
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token EOF

%type <Code.t> expr
%start <Code.doc> main

%%

let braces(p) == delimited(LBRACE, p, RBRACE)
let squares(p) == delimited(LSQUARE, p, RSQUARE)
let parens(p) == delimited(LPAREN, p, RPAREN)

let bvar := 
| x = TEXT; { [x] }

let binder == list(squares(bvar))

let node :=
| TITLE; ~ = arg; <Code.Title>
| AUTHOR; ~ = txt_arg; <Code.Author>
| DATE; ~ = txt_arg; <Code.Date>
| TEX_PACKAGE; ~ = txt_arg; <Code.TeXPackage>
| DEF; (~,~,~) = fun_spec; <Code.Def>
| ALLOC; ~ = IDENT; <Code.Alloc>
| TAXON; ~ = txt_arg; <Code.Taxon>
| META; ~ = txt_arg; ~ = arg; <Code.Meta>
| IMPORT; ~ = txt_arg; <Code.import_private>
| EXPORT; ~ = txt_arg; <Code.import_public>
| TAG; ~ = txt_arg; <Code.Tag>
| NAMESPACE; ~ = IDENT; ~ = arg; <Code.Namespace>

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
| SCOPE; ~ = arg; <Code.Scope>
| PUT; ~ = IDENT; ~ = arg; <Code.Put>
| DEFAULT; ~ = IDENT; ~ = arg; <Code.Default>
| GET; ~ = IDENT; <Code.Get>
| OPEN; ~ = IDENT; <Code.Open>

let expr == list(node)

let arg == braces(expr)
let txt_arg == braces(TEXT)
let fun_spec == ~ = IDENT; ~ = binder; ~ = arg; <>
  
let main := 
| ~ = expr; EOF; <>
