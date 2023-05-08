open Types

type globals = Symbol.t -> clo option

type resolution = 
  | Defined of clo
  | Undefined

let resolve globals env name = 
  match Env.find_opt name env with 
  | Some clo -> Defined clo
  | None ->
    match globals name with 
    | Some clo -> Defined clo 
    | None -> Undefined

let extend_env = 
  List.fold_right2 @@ fun x v ->
  Env.add (User x) (Val v)

let rec expand (globals : globals) env : Expr.t -> Sem.t = 
  function
  | [] -> []
  | Expr.Text txt :: rest -> 
    Sem.Text txt :: expand globals env rest
  | Expr.Group (Squares, title) :: Expr.Group (Parens, [Expr.Text addr]) :: rest -> 
    let title = expand globals env title in
    let link = Sem.Link {addr; title} in 
    link :: expand globals env rest
  | Expr.Group (delim, e) :: rest -> 
    Sem.Group (delim, expand globals env e) :: expand globals env rest
  | Expr.Math (mode, e) :: rest -> 
    Sem.Math (mode, expand globals env e) :: expand globals env rest
  | Expr.Tag name :: rest -> 
    expand_tag globals env name rest
  | Expr.Transclude name :: rest -> 
    Sem.Transclude name :: expand globals env rest
  | Expr.EmbedTeX e :: rest -> 
    Sem.EmbedTeX (expand globals env e) :: expand globals env rest
  | Expr.Let (name, xs, bdy) :: rest -> 
    let env' = Env.add (User name) (Clo (env, xs, bdy)) env in
    expand globals env' rest


and expand_no_op globals env msg =
  function 
  | Expr.Group (Braces, _) :: rest -> 
    expand globals env rest 
  | _ -> failwith msg

and expand_def_macro_binder globals env xs = 
  function 
  | Expr.Group (Squares, [Expr.Text x]) :: rest -> 
    expand_def_macro_binder globals env (xs @ [x]) rest
  | Expr.Group (Braces, bdy) :: rest -> 
    expand globals env rest
  | _ -> 
    failwith "expand_def_macro"

and expand_tag globals env name rest =
  match resolve globals env (User name) with 
  | Undefined -> expand_undefined_tag globals env name rest
  | Defined (Val v) -> v @ expand globals env rest
  | Defined (Clo (env', xs, body)) -> 
    let rest', extender = pop_args globals env (xs, rest) in
    expand globals (extender env') body @ expand globals env rest'

and pop_args globals env : string list * Expr.t ->  Expr.t * (env -> env) =
  function 
  | [], rest -> rest, Fun.id
  | x :: xs, Expr.Group (Braces, u) :: rest -> 
    let rest', extender = pop_args globals env (xs, rest) in
    let u' = expand globals env u in
    rest', fun env -> Env.add (User x) (Val u') (extender env)
  | _ -> 
    failwith "pop_args"

(* Just take only one argument, I guess *)
and expand_undefined_tag globals env name = 
  function 
  | Expr.Group (Braces, u) :: rest ->
    let u' = expand globals env u in
    Sem.Tag (name, [], [u']) :: expand globals env rest 
  | rest ->
    Sem.Tag (name, [], []) :: expand globals env rest
