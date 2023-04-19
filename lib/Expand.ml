open Types

type globals = string -> clo option

type resolution = 
  | Local of Sem.t 
  | Global of clo
  | Undefined

let resolve globals env name = 
  match Map.find_opt name env with 
  | Some v -> Local v 
  | None ->
    match globals name with 
    | Some clo -> Global clo 
    | None -> Undefined

let extend_env = 
  List.fold_right2 Map.add

let rec expand globals env = 
  function 
  | Syn.Text text ->
    Sem.Text text
  | Syn.Transclude addr -> 
    Sem.Transclude addr
  | Syn.Wikilink (title, dest) ->
    Sem.Wikilink (expand globals env title, dest) 
  | Syn.Seq xs -> 
    Sem.Seq (List.map (expand globals env) xs)
  | Syn.Tag (name, args) -> 
    let args' = args |> List.map @@ expand globals env in
    begin
      match resolve globals env name, args with 
      | Local v, [] -> v 
      | Global (Clo (env', xs, body)), _ -> 
        body |> expand globals @@ extend_env xs args' env'
      | Undefined, _ -> 
        Sem.Tag (name, [], args')
      | _ -> 
        failwith "expand"
    end
  | Syn.Math body -> 
    Sem.Math (expand globals env body)
  | Syn.Title _ | Syn.DefMacro _ | Syn.Import _ -> 
    Sem.Seq []
