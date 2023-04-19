%{
  open Types
  let rec get_text = 
    function 
    | Syn.Text txt -> txt 
    | Syn.Seq [x] -> get_text x
    | _ -> failwith "get_text"
  
  type frag = Syn.t list -> Syn.t list
%}

%token <string> TEXT MACRO
%token LBRACE RBRACE LSQUARE RSQUARE PIPE LLANGLE RRANGLE MATH
%token EOF
%type <frag> frag
%type <Syn.t> arg
%type <Syn.t> body
%start <Syn.t> main

%%
addr: 
| txt = TEXT 
  { String.trim txt }

frag:
| txt = TEXT 
  { fun cx -> Syn.Text txt :: cx }
    
| LSQUARE; title = body; PIPE; addr = addr; RSQUARE 
  { fun cx -> Syn.Wikilink (title, addr) :: cx }
    
| LLANGLE; addr = addr; RRANGLE 
  { fun cx -> Syn.Transclude addr :: cx }
  
| MATH; body = arg 
  { fun cx -> Syn.Math body :: cx }

| name = MACRO; args = list(arg)
  { match name with 
    | "title" -> fun cx -> Syn.Title (List.hd args) :: cx
    | "import" -> fun cx -> Syn.Import (get_text (List.hd args)) :: cx
    | "def" -> 
      fun cx ->
      let name = get_text @@ List.hd args in 
      let rest = List.rev @@ List.tl args in 
      let xs = List.rev_map get_text @@ List.tl rest in 
      let body = List.hd rest in
      Syn.DefMacro (name, xs, body) :: cx
    | "let" -> 
      fun cx -> 
      let name = get_text @@ List.hd args in 
      let rest = List.rev @@ List.tl args in 
      let xs = List.rev_map get_text @@ List.tl rest in 
      let body = List.hd rest in
      [Syn.Let (name, xs, body, Syn.Seq cx)]
    | _ -> fun cx -> Syn.Tag (name, args) :: cx }


body:
| frags = list(frag)
  { let result = List.fold_right (fun (x : frag) r -> x r) frags []  in 
    Syn.Seq result }

arg: 
| LBRACE bdy = body RBRACE
  { bdy }
  
main: 
| body = body; EOF
  { body }
