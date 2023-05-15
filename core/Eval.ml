open Base

module Env = Map.Make (String)

let rec eval ~env ~flenv : Syn.t -> Sem.t =
  function
  | [] -> []
  | Link {title; dest} :: rest ->
    let title = eval ~env ~flenv title in
    let link = Sem.Link {dest; title} in
    link :: eval ~env ~flenv rest
  | Math (mmode, e) :: rest ->
    Sem.Math (mmode, eval ~env ~flenv e) :: eval ~env ~flenv rest
  | Tag name :: rest ->
    eval_tag ~env ~flenv name rest
  | Transclude (tmode, name) :: rest ->
    Sem.Transclude (tmode, name) :: eval ~env ~flenv rest
  | EmbedTeX {packages; source} :: rest ->
    Sem.EmbedTeX {packages; source = eval ~env ~flenv source} :: eval ~env ~flenv rest
  | Block (title, body) :: rest ->
    Sem.Block (eval ~env ~flenv title, eval ~env ~flenv body)
    :: eval ~env ~flenv rest
  | Lam (xs, body) :: rest ->
    let rest', env' = pop_args ~env ~flenv (xs, rest) in
    eval ~env:env' ~flenv body @ eval ~env ~flenv rest'
  | Var x :: rest ->
    begin
      match Env.find_opt x env with
      | None -> failwith @@ Format.sprintf "Could not find variable named %s" x
      | Some v -> v @ eval ~env ~flenv rest
    end
  | Put (k, v, body) :: rest ->
    let body =
      let flenv = Env.add k (eval ~env ~flenv v) flenv in
      eval ~env ~flenv body
    in
    body @ eval ~env ~flenv rest
  | Default (k, v, body) :: rest ->
    let body =
      let flenv = if Env.mem k flenv then flenv else Env.add k (eval ~env ~flenv v) flenv in
      eval ~env ~flenv body
    in
    body @ eval ~env ~flenv rest  
  | Get key :: rest -> 
    begin
      match Env.find_opt key flenv with 
      | None -> failwith @@ Format.sprintf "Could not find fluid binding named %s" key
      | Some v -> v @ eval ~env ~flenv rest
    end
  | (Group _ :: _ | Text _ :: _) as rest->
    eval_textual ~env ~flenv [] rest

and eval_textual ~env ~flenv prefix : Syn.t -> Sem.t =
  function
  | Group (d, xs) :: rest ->
    let l, r =
      match d with
      | Braces -> "{", "}"
      | Squares -> "[", "]"
      | Parens -> "(", ")"
    in
    eval_textual ~env ~flenv (l :: prefix) @@ xs @ Text r :: rest
  | Text x :: rest ->
    eval_textual ~env ~flenv (x :: prefix) @@ rest
  | rest ->
    let txt = String.concat "" @@ List.rev prefix in
    Text txt :: eval ~env ~flenv rest

and eval_no_op ~env ~flenv msg =
  function
  | Syn.Group (Braces, _) :: rest ->
    eval ~env ~flenv rest
  | _ -> failwith msg

and pop_args ~env ~flenv : string list * Syn.t -> Syn.t * Sem.env =
  function
  | [], rest -> rest, env
  | x :: xs, Syn.Group (Braces, u) :: rest ->
    let rest', env = pop_args ~env ~flenv (xs, rest) in
    let u' = eval ~env ~flenv u in
    rest', Env.add x u' env
  | _ ->
    failwith "pop_args"

(* Just take only one argument, I guess *)
and eval_tag ~env ~flenv name =
  function
  | Syn.Group (Braces, u) :: rest ->
    let u' = eval ~env ~flenv u in
    Sem.Tag (name, [], [u']) :: eval ~env ~flenv rest
  | rest ->
    Sem.Tag (name, [], []) :: eval ~env ~flenv rest


let eval_doc (doc : Syn.doc) : Sem.doc =
  let fm, tree = doc in
  let env = Sem.empty in
  let flenv = Sem.empty in
  let tree = eval ~env ~flenv tree in
  let title = fm.title |> Option.map @@ eval ~env ~flenv in
  let metas =
    fm.metas |> List.map @@ fun (k, v) ->
    k, eval ~env ~flenv v
  in
  {title;
   body = tree;
   addr = fm.addr;
   taxon = fm.taxon;
   authors = fm.authors;
   date = fm.date;
   metas}
