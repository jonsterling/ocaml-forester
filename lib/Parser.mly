%{
  open Types
%}

%token <string> TEXT
%token <string> MACRO
%token <int> BVAR
%token DEFMACRO IMPORT
%token LBRACE RBRACE LSQUARE RSQUARE PIPE LLANGLE RRANGLE
%token EOF
%type <Syn.t> frag
%type <Syn.t> arg
%type <Syn.t> body
%start <Syn.t> main

%%
addr: 
| txt = TEXT 
  { String.trim txt }

frag:
| txt = TEXT 
  { Syn.Text txt }
    
| LSQUARE; title = body; PIPE; addr = addr; RSQUARE 
  { Syn.Wikilink (title, addr) }
    
| LLANGLE; addr = addr; RRANGLE 
  { Syn.Transclude addr }
  
| name = MACRO; args = list(arg)
  { Syn.Macro (name, args) }

body:

| frags = list(frag)
  { Syn.Seq frags }

arg: 
| LBRACE bdy = body RBRACE
  { bdy }

main: 
| body = body; EOF
  { body }
