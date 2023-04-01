%{
  let dispatch_macro ~name ~args = 
    match name with 
    | "b" -> new Tree.bold @@ List.hd args
    | "e" -> new Tree.emph @@ List.hd args
    | "p" -> new Tree.paragraph @@ List.hd args
    | "m" -> new Tree.math @@ List.hd args
    | _ -> new Tree.use_macro ~name ~args
%}

%token <string> TEXT
%token <string> MACRO
%token <int> BVAR
%token DEFMACRO IMPORT
%token LBRACE RBRACE LSQUARE RSQUARE PIPE
%token EOF
%type <Types.tree list -> Types.tree> frag
%type <Types.tree list -> Types.tree> arg
%type <Types.tree list -> Types.tree> body
%start <Types.tree list -> Types.tree> main

%%
frag: 

| txt = TEXT 
  { fun _ ->
    new Tree.plain_text txt }
    
| DEFMACRO; name = MACRO; body = arg;
  { fun env ->
    new Tree.def_macro ~name @@ fun args -> 
    body @@ args @ env }
  
| ix = BVAR
  { fun env ->
    List.nth env @@ ix - 1 }
  
| IMPORT; LBRACE; addr = TEXT; RBRACE 
  { fun _ ->
    new Tree.import_macros addr }
  
| LSQUARE; title = body; PIPE; addr = TEXT; RSQUARE 
  { fun env ->
    new Tree.wikilink (title env) addr }
  
| name = MACRO; args = list(arg)
  { fun env ->
    let args = List.map (fun arg -> arg env) args in
    dispatch_macro ~name ~args }

body:

| frags = list(frag)
  { fun env ->
    new Tree.glue ~sep:" " @@
    List.map (fun frag -> frag env) frags }

arg: 
| LBRACE bdy = body RBRACE
  { bdy }

main: 
| body = body; EOF
  { body }
