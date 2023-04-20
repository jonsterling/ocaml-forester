%{
  open Types
  
  let get_text =
    function
    | [Syn.Text txt] -> txt
    | _ -> failwith "get_text"

  type frag = Syn.t -> Syn.t

  let extract_macro_binder args =
    let name = get_text @@ List.hd args in
    let rest = List.rev @@ List.tl args in
    let xs = List.rev_map get_text @@ List.tl rest in
    let body = List.hd rest in
    name, xs, body

%}

%token <string> TEXT MACRO
%token LBRACE RBRACE LSQUARE RSQUARE PIPE MATH BEGIN_TEX END_TEX
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
  { List.cons @@ Syn.Text txt }

| LSQUARE; title = body; PIPE; addr = addr; RSQUARE
  { List.cons @@ Syn.Wikilink (title, addr)  }

| MATH; body = arg
  { List.cons @@ Syn.Math body }

| name = MACRO; args = list(arg)
  { match name with
    | "title" ->
      List.cons @@ Syn.Title (List.hd args)
    | "import" ->
      List.cons @@ Syn.Import (get_text (List.hd args))
    | "def" ->
      let name, xs, body = extract_macro_binder args in
      List.cons @@ Syn.DefMacro (name, xs, body)
    | "let" ->
      fun cx ->
      let name, xs, body = extract_macro_binder args in
      [Syn.Let (name, xs, body, cx)]
    | "tex" -> 
      List.cons @@ Syn.EmbedTeX (List.hd args)
    | "transclude" -> 
      List.cons @@ Syn.Transclude (get_text (List.hd args))
    | _ ->
      List.cons @@ Syn.Tag (name, [], args) }


body:
| frags = list(frag)
  { List.fold_right Fun.id frags [] }

arg:
| LBRACE; bdy = body; RBRACE
  { bdy }
| BEGIN_TEX; body = body; END_TEX
  { body }

main:
| body = body; EOF
  { body }
