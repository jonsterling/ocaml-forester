%{
  open Prelude
  open Core

  let full_transclude x = Code.Transclude (Full, x)
  let splice_transclude x = Code.Transclude (Spliced, x)
  let collapse_transclude x = Code.Transclude (Collapsed, x)
  
  let full_query_tree (title, query) = Code.Query (title, Full, query)
  let collapsed_query_tree (title, query) = Code.Query (title, Collapsed, query)
  let spliced_query_tree (title, query) = Code.Query (title, Spliced, query)

%}

%token <string> TEXT 
%token <string list> IDENT
%token TITLE IMPORT EXPORT DEF TAXON AUTHOR TEX_PACKAGE TAG DATE NAMESPACE LET TEX BLOCK META OPEN
%token TRANSCLUDE TRANSCLUDE_STAR TRANSCLUDE_AT SCOPE PUT GET DEFAULT ALLOC 
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token QUERY_AND QUERY_OR QUERY_AUTHOR QUERY_TAG QUERY_TAXON QUERY_META
%token QUERY_TREE QUERY_TREE_STAR QUERY_TREE_AT
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
| QUERY_TREE; ~ = arg; ~ = braces(query); <full_query_tree>
| QUERY_TREE_STAR; ~ = arg; ~ = braces(query); <collapsed_query_tree>
| QUERY_TREE_AT; ~ = arg; ~ = braces(query); <spliced_query_tree>

let eat_text == option(TEXT)

let query0 := 
| QUERY_AUTHOR; ~ = txt_arg; <Query.Author>
| QUERY_TAG; ~ = txt_arg; <Query.Tag>
| QUERY_TAXON; ~ = txt_arg; <Query.Taxon>
| QUERY_AND; ~ = braces(queries); <Query.And>
| QUERY_OR; ~ = braces(queries); <Query.Or>
| QUERY_META; k = txt_arg; v = txt_arg; <Query.Meta>

let queries := 
| TEXT; { [] }
| qs = queries; q = query0; _ = eat_text; {qs @ [q]}

let query := _ = eat_text; q = query0; eat_text; {q}

let expr == list(node)

let arg == braces(expr)
let txt_arg == braces(TEXT)
let fun_spec == ~ = IDENT; ~ = binder; ~ = arg; <>
  
let main := 
| ~ = expr; EOF; <>
