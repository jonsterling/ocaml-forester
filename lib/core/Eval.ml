open Base
open Bwd

module LexEnv = Algaeff.Reader.Make (struct type env = Sem.t Env.t end)
module DynEnv = Algaeff.Reader.Make (struct type env = Sem.t Env.t end)

let get_transclusion_opts () =
  let dynenv = DynEnv.read () in
  let title_override = Env.find_opt Expand.Builtins.Transclude.title_sym dynenv in
  let get_bool key default =
    match Env.find_opt key dynenv with
    | Some [Sem.Text "true"] -> true
    | Some [Sem.Text "false"] -> false
    | _ -> default
  in
  let expanded = get_bool Expand.Builtins.Transclude.expanded_sym true in
  let show_heading = get_bool Expand.Builtins.Transclude.show_heading_sym true in
  let toc = get_bool Expand.Builtins.Transclude.toc_sym true in
  let numbered = get_bool Expand.Builtins.Transclude.numbered_sym true in
  let show_metadata = get_bool Expand.Builtins.Transclude.show_metadata_sym false in
  Sem.{title_override; toc; show_heading; expanded; numbered; show_metadata}

let rec eval : Syn.t -> Sem.t =
  function
  | [] -> []
  | Link {title; dest} :: rest ->
    let title = eval title in
    let dest = Sem.string_of_nodes @@ eval_textual [] dest in
    let link = Sem.Link {dest; title} in
    link :: eval rest
  | Math (mmode, e) :: rest ->
    Sem.Math (mmode, eval e) :: eval rest
  | Tag name :: rest ->
    eval_tag name rest
  | Transclude addr :: rest ->
    let opts = get_transclusion_opts () in
    Sem.Transclude (opts, addr):: eval rest
  | Query query :: rest ->
    let opts = get_transclusion_opts () in
    let opts =
      match opts.title_override with
      | None -> {opts with show_heading = false; toc = false}
      | Some _ -> opts
    in
    let query = Query.map eval query in
    Sem.Query (opts, query) :: eval rest
  | Embed_TeX {packages; source} :: rest ->
    Sem.Embed_TeX {packages; source = eval source} :: eval rest
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
  | (Group _ :: _ | Text _ :: _) as rest ->
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


(* Just take only one argument, I guess *)
and eval_tag tag =
  let rec parse_attrs tag attrs =
    function
    | Syn.Group (Squares, [Text key]) :: Group (Braces, [Text value]) :: rest ->
      let attrs = Bwd.Snoc (attrs, (key, value)) in
      parse_attrs tag attrs rest
    | Syn.Group (Braces, body) :: rest ->
      let attrs = Bwd.to_list attrs in
      Sem.Tag (tag, attrs, eval body) :: eval rest
    | rest ->
      let attrs = Bwd.to_list attrs in
      Sem.Tag (tag, attrs, []) :: eval rest
  in
  parse_attrs tag Bwd.Emp

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
   addr = Some fm.addr;
   taxon = fm.taxon;
   authors = fm.authors;
   date = fm.date;
   tags = fm.tags;
   metas}
