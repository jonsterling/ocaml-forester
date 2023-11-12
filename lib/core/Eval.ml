open Base
open Bwd

module LexEnv = Algaeff.Reader.Make (struct type env = Sem.t Env.t end)
module DynEnv = Algaeff.Reader.Make (struct type env = Sem.t Env.t end)
module PkgEnv = Algaeff.Reader.Make (struct type env = string list end)

let get_transclusion_opts () =
  let dynenv = DynEnv.read () in
  let title_override = Env.find_opt Expand.Builtins.Transclude.title_sym dynenv in
  let taxon_override =
    match Env.find_opt Expand.Builtins.Transclude.taxon_sym dynenv with
    | Some [{value = Sem.Text text; _}] -> Some text
    | _ -> None
  in
  let get_bool key default =
    match Env.find_opt key dynenv with
    | Some [{value = Sem.Text "true"; _}] -> true
    | Some [{value = Sem.Text "false"; _}] -> false
    | _ -> default
  in
  let expanded = get_bool Expand.Builtins.Transclude.expanded_sym true in
  let show_heading = get_bool Expand.Builtins.Transclude.show_heading_sym true in
  let toc = get_bool Expand.Builtins.Transclude.toc_sym true in
  let numbered = get_bool Expand.Builtins.Transclude.numbered_sym true in
  let show_metadata = get_bool Expand.Builtins.Transclude.show_metadata_sym false in
  Sem.{title_override; taxon_override; toc; show_heading; expanded; numbered; show_metadata}

let rec eval : Syn.t -> Sem.t =
  function
  | [] -> []
  | node :: rest ->
    eval_node node rest

and eval_node : Syn.node Range.located -> Syn.t -> Sem.t =
  fun node rest ->
  match node.value with
  | Link {title; dest} ->
    let title = Option.map eval title in
    let dest = Sem.string_of_nodes @@ eval_textual [] dest in
    let link = Range.locate_opt node.loc @@ Sem.Link {dest; title} in
    link :: eval rest
  | Math (mmode, e) ->
    Range.locate_opt node.loc (Sem.Math (mmode, eval e)) :: eval rest
  | Prim (p, body) ->
    Range.locate_opt node.loc (Sem.Prim (p, eval body)) :: eval rest
  | Xml_tag (name, attrs, body) ->
    let attrs =
      attrs |> List.map @@ fun (k, v) ->
      k, eval v
    in
    let xml = Sem.Xml_tag (name, attrs, eval body) in
    Range.locate_opt node.loc xml :: eval rest
  | Unresolved name ->
    let cmd = Sem.Unresolved name in
    Range.locate_opt node.loc cmd :: eval rest
  | Transclude addr ->
    let opts = get_transclusion_opts () in
    Range.locate_opt node.loc (Sem.Transclude (opts, addr)) :: eval rest
  | If_tex (x , y) ->
    let x = eval x in
    let y = eval y in
    Range.locate_opt node.loc (Sem.If_tex (x, y)) :: eval rest
  | Query query ->
    let opts = get_transclusion_opts () in
    let opts =
      match opts.title_override with
      | None -> {opts with show_heading = false; toc = false}
      | Some _ -> opts
    in
    let query = Query.map eval query in
    Range.locate_opt node.loc (Sem.Query (opts, query)) :: eval rest
  | Embed_tex {source} ->
    let packages = PkgEnv.read () in
    Range.locate_opt node.loc (Sem.Embed_tex {packages; source = eval source}) :: eval rest
  | Block (title, body) ->
    Range.locate_opt node.loc (Sem.Block (eval title, eval body)) :: eval rest
  | Lam (xs, body) ->
    let rec loop xs rest =
      match xs, rest with
      | [], rest -> eval body, rest
      | x :: xs, Range.{value = Syn.Group (Braces, u); loc = loc'} :: rest ->
        LexEnv.scope (Env.add x (eval u)) @@ fun () ->
        loop xs rest
      | _ ->
        Reporter.fatalf Type_error ?loc:node.loc
          "expected function to be applied to `%i` additional arguments"
          (List.length xs)
    in
    let body, rest = loop xs rest in
    body @ eval rest
  | Thunk body ->
    let env = LexEnv.read () in
    Range.locate_opt node.loc (Sem.Clo (body, env)) :: eval rest
  | Force body ->
    let not_whitespace node =
      match Range.(node.value) with
      | Sem.Text txt -> String.trim txt <> ""
      | _ -> true
    in
    begin
      match List.filter not_whitespace @@ eval body with
      | [{value = Clo (syn, env); _}] ->
        LexEnv.scope (fun _ -> env) @@ fun () ->
        eval syn
      | body ->
        Reporter.fatalf ?loc:node.loc Type_error
          "tried to force non-closure: %a" Sem.pp body
    end
  | Var x ->
    begin
      match Env.find_opt x @@ LexEnv.read () with
      | None ->
        Reporter.fatalf ?loc:node.loc Resolution_error
          "could not find variable named %a"
          Symbol.pp x
      | Some v -> v @ eval rest
    end
  | Put (k, v, body) ->
    let body =
      DynEnv.scope (Env.add k @@ eval v) @@ fun () ->
      eval body
    in
    body @ eval rest
  | Default (k, v, body) ->
    let body =
      let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval v) flenv in
      DynEnv.scope upd @@ fun () ->
      eval body
    in
    body @ eval rest
  | Get key ->
    begin
      let env = DynEnv.read () in
      match Env.find_opt key @@ DynEnv.read () with
      | None ->
        Eio.traceln "getting %a from %a" Symbol.pp key (Env.pp Sem.pp) env;
        Reporter.fatalf ?loc:node.loc Resolution_error
          "could not find fluid binding named %a"
          Symbol.pp key
      | Some v -> v @ eval rest
    end
  | Group _ | Text _ ->
    eval_textual [] @@ node :: rest

and eval_textual prefix : Syn.t -> Sem.t =
  function
  | {value = Group (d, xs); _} :: rest ->
    let l, r =
      match d with
      | Braces -> "{", "}"
      | Squares -> "[", "]"
      | Parens -> "(", ")"
    in
    eval_textual (l :: prefix) @@ xs @ Asai.Range.locate_opt None (Syn.Text r) :: rest
  | {value = Text x; _} :: rest ->
    eval_textual (x :: prefix) @@ rest
  | rest ->
    let txt = String.concat "" @@ List.rev prefix in
    Range.locate_opt None (Sem.Text txt) :: eval rest


let eval_doc (doc : Syn.doc) : Sem.doc =
  let fm, tree = doc in
  LexEnv.run ~env:Env.empty @@ fun () ->
  DynEnv.run ~env:Env.empty @@ fun () ->
  PkgEnv.run ~env:fm.tex_packages @@ fun () ->
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
