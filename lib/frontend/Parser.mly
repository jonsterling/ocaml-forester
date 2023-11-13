%{
  open Prelude
  open Core
%}

%token <string> TEXT
%token <string> WHITESPACE
%token <string list> IDENT
%token <Core.Prim.t> PRIM
%token TITLE IMPORT EXPORT DEF TAXON AUTHOR TEX_PACKAGE TAG DATE NAMESPACE LET TEX BLOCK META OPEN
%token THUNK FORCE OBJECT PATCH CALL
%token TRANSCLUDE SCOPE PUT GET DEFAULT ALLOC IF_TEX XML_TAG
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token QUERY_AND QUERY_OR QUERY_AUTHOR QUERY_TAG QUERY_TAXON QUERY_META
%token QUERY_TREE
%token EOF

%start <Code.t> main

%%

let locate(p) ==
| x = p; { Asai.Range.locate_lex $loc x }

let braces(p) == delimited(LBRACE, p, RBRACE)
let squares(p) == delimited(LSQUARE, p, RSQUARE)
let parens(p) == delimited(LPAREN, p, RPAREN)

let bvar :=
| x = TEXT; { [x] }

let binder == list(squares(bvar))

let ws_or(p) :=
| WHITESPACE; { [] }
| x = p; { [x] }

let ws_list(p) := flatten(list(ws_or(p)))

let textual_node :=
| ~ = TEXT; <Code.Text>
| ~ = WHITESPACE; <Code.Text>
| ~ = head_node; <Fun.id>

let code_expr == ws_list(locate(head_node))
let textual_expr == list(locate(textual_node))

let head_node :=
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
| NAMESPACE; ~ = IDENT; ~ = braces(code_expr); <Code.Namespace>
| TRANSCLUDE; ~ = txt_arg; <Code.Transclude>
| LET; (~,~,~) = fun_spec; <Code.Let>
| TEX; ~ = arg; <Code.Embed_tex>
| THUNK; ~ = arg; <Code.Thunk>
| FORCE; ~ = braces(code_expr); <Code.Force>
| IF_TEX; x = arg; y = arg; <Code.If_tex>
| BLOCK; x = arg; y = arg; <Code.Block>
| ~ = IDENT; <Code.Ident>
| SCOPE; ~ = arg; <Code.Scope>
| PUT; ~ = IDENT; ~ = arg; <Code.Put>
| DEFAULT; ~ = IDENT; ~ = arg; <Code.Default>
| GET; ~ = IDENT; <Code.Get>
| OPEN; ~ = IDENT; <Code.Open>
| QUERY_TREE; ~ = braces(query); <Code.Query>
| XML_TAG; ~ = txt_arg; ~ = list(xml_attr); ~ = arg; <Code.Xml_tag>
| OBJECT; ~ = option(squares(bvar)); ~ = braces(ws_list(struct_field)); <Code.Object>
| PATCH; ~ = braces(code_expr); ~ = option(squares(bvar)); ~ = braces(ws_list(struct_field)); <Code.Patch>
| CALL; ~ = braces(code_expr); ~ = txt_arg; <Code.Call>
| ~ = PRIM; ~ = arg; <Code.Prim>
| ~ = delimited(HASH_LBRACE, textual_expr, RBRACE); <Code.inline_math>
| ~ = delimited(HASH_HASH_LBRACE, textual_expr, RBRACE); <Code.display_math>
| ~ = braces(textual_expr); <Code.braces>
| ~ = squares(textual_expr); <Code.squares>
| ~ = parens(textual_expr); <Code.parens>

let struct_field :=
| k = squares(TEXT); list(WHITESPACE); v = arg; { k, v }

let xml_attr :=
| k = squares(TEXT); v = arg; { (k, v) }


let query0 :=
| QUERY_AUTHOR; ~ = arg; <Query.Author>
| QUERY_TAG; ~ = arg; <Query.Tag>
| QUERY_TAXON; ~ = arg; <Query.Taxon>
| QUERY_AND; ~ = braces(queries); <Query.And>
| QUERY_OR; ~ = braces(queries); <Query.Or>
| QUERY_META; k = txt_arg; v = arg; <Query.Meta>

let queries == ws_list(query0)

let query := list(WHITESPACE); q = query0; list(WHITESPACE); {q}

let ws_or_text :=
| x = TEXT; { x }
| x = WHITESPACE; { x }

let wstext :=
| xs = list(ws_or_text); { String.concat "" xs }

let arg == braces(textual_expr)
let txt_arg == braces(wstext)
let fun_spec == ~ = IDENT; ~ = binder; ~ = arg; <>

let main :=
| ~ = ws_list(locate(head_node)); EOF; <>
