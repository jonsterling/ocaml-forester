%{
  open Prelude
  open Core
%}

%token <string> TEXT
%token <string list> IDENT
%token <Core.Prim.t> PRIM
%token TITLE IMPORT EXPORT DEF TAXON AUTHOR TEX_PACKAGE TAG DATE NAMESPACE LET TEX BLOCK META OPEN
%token TRANSCLUDE SCOPE PUT GET DEFAULT ALLOC IF_TEX XML_TAG
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token QUERY_AND QUERY_OR QUERY_AUTHOR QUERY_TAG QUERY_TAXON QUERY_META
%token QUERY_TREE
%token EOF

%type <Code.t> expr
%start <Core.Code.t> main

%%

%inline
locate(X):
  | e = X
    { Asai.Range.locate_lex $loc e }

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
| TEX_PACKAGE; ~ = txt_arg; <Code.TeX_package>
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
| TRANSCLUDE; ~ = txt_arg; <Code.Transclude>
| LET; (~,~,~) = fun_spec; <Code.Let>
| TEX; ~ = arg; <Code.Embed_tex>
| IF_TEX; x = arg; y = arg; <Code.If_tex>
| BLOCK; x = arg; y = arg; <Code.Block>
| ~ = IDENT; <Code.Ident>
| ~ = TEXT; <Code.Text>
| SCOPE; ~ = arg; <Code.Scope>
| PUT; ~ = IDENT; ~ = arg; <Code.Put>
| DEFAULT; ~ = IDENT; ~ = arg; <Code.Default>
| GET; ~ = IDENT; <Code.Get>
| OPEN; ~ = IDENT; <Code.Open>
| QUERY_TREE; ~ = braces(query); <Code.Query>
| XML_TAG; ~ = txt_arg; ~ = list(xml_attr); ~ = arg; <Code.Xml_tag>
| ~ = PRIM; ~ = arg; <Code.Prim>

let xml_attr :=
| k = squares(TEXT); v = arg; { (k, v) }

let eat_text == option(TEXT)

let query0 :=
| QUERY_AUTHOR; ~ = arg; <Query.Author>
| QUERY_TAG; ~ = arg; <Query.Tag>
| QUERY_TAXON; ~ = arg; <Query.Taxon>
| QUERY_AND; ~ = braces(queries); <Query.And>
| QUERY_OR; ~ = braces(queries); <Query.Or>
| QUERY_META; k = txt_arg; v = arg; <Query.Meta>

let queries :=
| TEXT; { [] }
| qs = queries; q = query0; _ = eat_text; {qs @ [q]}

let query := _ = eat_text; q = query0; eat_text; {q}

let expr == list(locate(node))

let arg == braces(expr)
let txt_arg == braces(TEXT)
let fun_spec == ~ = IDENT; ~ = binder; ~ = arg; <>

let main :=
| ~ = expr; EOF; <>
