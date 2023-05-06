%{
  open Types
  
  let get_text =
    function
    | [Syn.Text txt] -> txt
    | _ -> failwith "get_text"

  type frag = Syn.t -> Syn.t

  let extract_macro_binder args =
    let rargs = List.rev args in
    let xs = List.rev_map get_text @@ List.tl rargs in
    let body = List.hd rargs in
    xs, body

%}

%token <string> TEXT MACRO
%token TITLE IMPORT DEF LET TEX TRANSCLUDE TAXON 
%token LBRACE RBRACE LSQUARE RSQUARE PIPE MATH BEGIN_TEX END_TEX GROUP
%token EOF
%type <frag> frag
%type <Syn.t> arg
%type <Syn.t> body
%start <Syn.t> main

%%
addr:
| txt = TEXT
  { String.trim txt }

wikilink_title:
| PIPE; title = body 
  { title }

frag:
| txt = TEXT
  { List.cons @@ Syn.Text txt }

| LSQUARE LSQUARE; addr = addr; title = option(wikilink_title); RSQUARE RSQUARE
  { List.cons @@ Syn.Wikilink {title; addr}  }
  
| MATH; body = arg
  { List.cons @@ Syn.Math body }
  
| TITLE; arg = arg
  { List.cons @@ Syn.Title arg }
  
| TAXON; taxon = txt_arg
  { List.cons @@ Syn.Taxon taxon }

| IMPORT; addr = txt_arg; 
  { List.cons @@ Syn.Import addr }
  
| DEF; name = txt_arg; args = list(arg)
  { let xs, body = extract_macro_binder args in 
    List.cons @@ Syn.DefMacro (name, xs, body) }
    
| TRANSCLUDE; addr = txt_arg 
  { List.cons @@ Syn.Transclude addr }

| TEX; arg = arg 
  { List.cons @@ Syn.EmbedTeX arg }

| GROUP; arg = arg 
  { List.cons @@ Syn.Group arg }  

| LET; name = txt_arg; args = list(arg) 
  { fun cx ->
    let xs, body = extract_macro_binder args in 
    [Syn.Let (name, xs, body, cx)] }

| name = MACRO; args = list(arg)
  { List.cons @@ Syn.Tag (name, [], args) }

body:
| frags = list(frag)
  { List.fold_right Fun.id frags [] }

txt_arg: 
| LBRACE; txt = TEXT; RBRACE 
  { txt }

arg:
| LBRACE; bdy = body; RBRACE
  { bdy }
| BEGIN_TEX; body = body; END_TEX
  { body }

main:
| body = body; EOF
  { body }
