open Base

module LexEnv = Algaeff.Reader.Make (struct type env = Sem.t Env.t end)
module DynEnv = Algaeff.Reader.Make (struct type env = Sem.t Env.t end)

let rec eval : Syn.t -> Sem.t =
  function
  | [] -> []
  | Link {title; dest} :: rest ->
    let title = eval title in
    let link = Sem.Link {dest; title} in
    link :: eval rest
  | Math (mmode, e) :: rest ->
    Sem.Math (mmode, eval e) :: eval rest
  | Tag name :: rest ->
    eval_tag name rest
  | Transclude (tmode, name) :: rest ->
    Sem.Transclude (tmode, name) :: eval rest
  | Bibliography (title, mode, query) :: rest -> 
    let title = eval title in
    Sem.Bibliography (title, mode, query) :: eval rest
  | EmbedTeX {packages; source} :: rest ->
    Sem.EmbedTeX {packages; source = eval source} :: eval rest
  | Block (title, body) :: rest ->
    Sem.Block (eval title, eval body) :: eval rest
  | Lam (xs, body) :: rest ->
    let rec loop xs rest =
      match xs, rest with
      | [], rest -> eval body, rest
      | x :: xs, Syn.Group (Braces, u) :: rest ->
        LexEnv.scope (Env.add x (eval u)) @@ fun () ->
        loop xs rest
      | _ ->
        failwith "eval/Lam"
    in
    let body, rest = loop xs rest in
    body @ eval rest
  | Var x :: rest ->
    begin
      match Env.find_opt x @@ LexEnv.read () with
      | None -> failwith @@ Format.asprintf "Could not find variable named %a" Symbol.pp x
      | Some v -> v @ eval rest
    end
  | Put (k, v, body) :: rest ->
    let body =
      DynEnv.scope (Env.add k @@ eval v) @@ fun () ->
      eval body
    in
    body @ eval rest
  | Default (k, v, body) :: rest ->
    let body =
      let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval v) flenv in
      DynEnv.scope upd @@ fun () ->
      eval body
    in
    body @ eval rest
  | Get key :: rest ->
    begin
      match Env.find_opt key @@ DynEnv.read () with
      | None -> failwith @@ Format.asprintf "Could not find fluid binding named %a" Symbol.pp key
      | Some v -> v @ eval rest
    end
  | (Group _ :: _ | Text _ :: _) as rest->
    eval_textual [] rest

and eval_textual prefix : Syn.t -> Sem.t =
  function
  | Group (d, xs) :: rest ->
    let l, r =
      match d with
      | Braces -> "{", "}"
      | Squares -> "[", "]"
      | Parens -> "(", ")"
    in
    eval_textual (l :: prefix) @@ xs @ Text r :: rest
  | Text x :: rest ->
    eval_textual (x :: prefix) @@ rest
  | rest ->
    let txt = String.concat "" @@ List.rev prefix in
    Text txt :: eval rest

and eval_no_op ~env ~flenv msg =
  function
  | Syn.Group (Braces, _) :: rest ->
    eval rest
  | _ -> failwith msg



(* Just take only one argument, I guess *)
and eval_tag  name =
  function
  | Syn.Group (Braces, u) :: rest ->
    let u' = eval u in
    Sem.Tag (name, u') :: eval rest
  | rest ->
    Sem.Tag (name, []) :: eval rest


let eval_doc (doc : Syn.doc) : Sem.doc =
  let fm, tree = doc in
  LexEnv.run ~env:Env.empty @@ fun () ->
  DynEnv.run ~env:Env.empty @@ fun () ->
  let tree = eval tree in
  let title = Option.map eval fm.title in
  let metas =
    fm.metas |> List.map @@ fun (k, v) ->
    k, eval v
  in
  let open Sem in
  {title;
   body = tree;
   addr = fm.addr;
   taxon = fm.taxon;
   authors = fm.authors;
   date = fm.date;
   tags = fm.tags;
   metas}
