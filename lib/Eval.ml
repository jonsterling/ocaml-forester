open Types

let extend_env = 
  List.fold_right2 @@ fun x v ->
  Env.add x v

let rec eval env : Term.t -> Sem.t = 
  function
  | [] -> []
  | Link {title; dest} :: rest -> 
    let title = eval env title in
    let link = Sem.Link {dest; title} in 
    link :: eval env rest
  | Math (mmode, e) :: rest -> 
    Sem.Math (mmode, eval env e) :: eval env rest
  | Tag name :: rest -> 
    eval_tag env name rest
  | Transclude (tmode, name) :: rest -> 
    Sem.Transclude (tmode, name) :: eval env rest
  | EmbedTeX {packages; source} :: rest -> 
    Sem.EmbedTeX {packages; source = eval env source} :: eval env rest
  | Block (title, body) :: rest -> 
    Sem.Block (eval env title, eval env body)
    :: eval env rest
  | Lam (xs, body) :: rest -> 
    let rest', env' = pop_args env (xs, rest) in
    eval env' body @ eval env rest'
  | Var x :: rest -> 
    begin 
      match Env.find_opt x env with 
      | None -> failwith @@ Format.sprintf "Could not find variable named %s" x
      | Some v -> v @ eval env rest
    end
  | rest -> 
    eval_textual env [] rest

and eval_textual env prefix : Term.t -> Sem.t =
  function 
  | Group (d, xs) :: rest -> 
    let l, r = 
      match d with 
      | Braces -> "{", "}"
      | Squares -> "[", "]"
      | Parens -> "(", ")"
    in
    eval_textual env (l :: prefix) @@ xs @ Text r :: rest 
  | Text x :: rest -> 
    eval_textual env (x :: prefix) @@ rest
  | rest ->
    let txt = String.concat "" @@ List.rev prefix in
    Text txt :: eval env rest

and eval_no_op env msg =
  function 
  | Term.Group (Braces, _) :: rest -> 
    eval env rest 
  | _ -> failwith msg

and pop_args env : string list * Term.t -> Term.t * Sem.env =
  function 
  | [], rest -> rest, env
  | x :: xs, Term.Group (Braces, u) :: rest -> 
    let rest', env = pop_args env (xs, rest) in
    let u' = eval env u in
    rest', Env.add x u' env
  | _ -> 
    failwith "pop_args"

(* Just take only one argument, I guess *)
and eval_tag env name = 
  function 
  | Term.Group (Braces, u) :: rest ->
    let u' = eval env u in
    Sem.Tag (name, [], [u']) :: eval env rest 
  | rest ->
    Sem.Tag (name, [], []) :: eval env rest
