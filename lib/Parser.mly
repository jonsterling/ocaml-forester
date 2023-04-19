%{
  open Types
  let rec get_text = 
    function 
    | Syn.Text txt -> txt 
    | Syn.Seq [x] -> get_text x
    | _ -> failwith "get_text"
%}

%token <string> TEXT MACRO VAR
%token LBRACE RBRACE LSQUARE RSQUARE PIPE LLANGLE RRANGLE MATH
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
  
| MATH; body = arg 
  { Syn.Math body }

| name = MACRO; args = list(arg)
  { match name with 
    | "title" -> Syn.Title (List.hd args)
    | "import" -> Syn.Import (get_text (List.hd args))
    | "def" -> 
      let name = get_text @@ List.hd args in 
      let rest = List.rev @@ List.tl args in 
      let xs = List.rev_map get_text @@ List.tl rest in 
      let body = List.hd rest in
      Syn.DefMacro (name, xs, body)
    | _ -> Syn.Tag (name, args) }


body:
| frags = list(frag)
  { Syn.Seq frags }

arg: 
| LBRACE bdy = body RBRACE
  { bdy }
  
main: 
| body = body; EOF
  { body }
