open Types

let rec expand macros env = 
  function 
  | Syn.Text text ->
    Sem.Text text
  | Syn.Transclude addr -> 
    Sem.Transclude addr
  | Syn.Wikilink (title, dest) ->
    Sem.Wikilink (expand macros env title, dest) 
  | Syn.Seq xs -> 
    Sem.Seq (List.map (expand macros env) xs)
  | Syn.Tag (name, args) -> 
    let args' = List.map (expand macros env) args in
    begin
      match Map.find_opt name env, args with 
      | Some v, [] -> v
      | None, _ -> 
        begin
          match Hashtbl.find_opt macros name with 
          | Some (Clo (env', xs, body)) -> 
            let env'' = List.fold_right2 Map.add xs args' env' in
            expand macros env'' body
          | None ->
            Sem.Tag (name, [], args')
        end
      | _ -> failwith "expand"
    end
  | Syn.Math body -> 
    let body' = expand macros env body in 
    Sem.Math body'
  | Syn.Title _ | Syn.DefMacro _ | Syn.Import _ -> 
    Sem.Seq []
