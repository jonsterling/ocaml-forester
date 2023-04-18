%{
  open Types
  let rec get_text = 
    function 
    | Syn.Text txt -> txt 
    | Syn.Seq [x] -> get_text x
    | _ -> failwith "get_text"
%}

%token <string> TEXT
%token <string> MACRO TAG
%token <int> BVAR
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
  { match name with 
    | "title" -> Syn.Title (List.hd args)
    | "import" -> Syn.Import (get_text (List.hd args))
    | "def" -> Syn.DefMacro (get_text (List.hd args), List.nth args 1)
    | _ -> Syn.Macro (name, args) }
    
 | name = TAG; body = arg
   { Syn.Tag (name, [], body) }
    
 | bvar = BVAR 
   { Syn.BVar bvar }
 

body:
| frags = list(frag)
  { Syn.Seq frags }

arg: 
| LBRACE bdy = body RBRACE
  { bdy }
  
main: 
| body = body; EOF
  { body }
