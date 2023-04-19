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
  List.fold_right2 (fun x v -> Env.add (User x) (Val v))

let rec expand_node globals env = 
  function 
  | Syn.Text text ->
    [Sem.Text text]
  | Syn.Transclude addr -> 
    [Sem.Transclude addr]
  | Syn.Wikilink (title, dest) ->
    [Sem.Wikilink (expand_nodes globals env title, dest)]
  | Syn.Tag (name, args) -> 
    let args' = args |> List.map @@ expand_nodes globals env in
    begin
      match resolve globals env (User name), args with 
      | Defined (Val v), [] -> v
      | Defined (Clo (env', xs, body)), _ -> 
        body |> expand_nodes globals @@ extend_env xs args' env'
      | Undefined, _ -> 
        [Sem.Tag (name, [], args')]
      | _ -> 
        failwith "expand"
    end
  | Syn.Math body -> 
    [Sem.Math (expand_nodes globals env body)]
  | Syn.Let (name, xs, body, rest) -> 
    let env' = Env.add (User name) (Clo (env, xs, body)) env in
    expand_nodes globals env' rest
  | Syn.Title _ | Syn.DefMacro _ | Syn.Import _ -> 
    []

and expand_nodes globals env =
  List.concat_map @@ expand_node globals env
