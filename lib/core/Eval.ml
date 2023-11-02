open Base
open Bwd

module LexEnv = Algaeff.Reader.Make (struct type env = Sem.t Env.t end)
module DynEnv = Algaeff.Reader.Make (struct type env = Sem.t Env.t end)

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
  | Tag name ->
    eval_tag node.loc name rest
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
  | Embed_tex {packages; source} ->
    Range.locate_opt node.loc (Sem.Embed_tex {packages; source = eval source}) :: eval rest
  | Block (title, body) ->
    Range.locate_opt node.loc (Sem.Block (eval title, eval body)) :: eval rest
  | Lam (xs, body) ->
    let rec loop loc xs rest =
      match xs, rest with
      | [], rest -> eval body, rest
      | x :: xs, Range.{value = Syn.Group (Braces, u); loc = loc'} :: rest ->
        LexEnv.scope (Env.add x (eval u)) @@ fun () ->
        loop (Range.merge_ranges_opt loc loc') xs rest
      | _ ->
        Reporter.fatalf TypeError ?loc
          "expected function to be applied to `%i` arguments"
          (List.length xs)
    in
    let body, rest = loop node.loc xs rest in
    body @ eval rest
  | Var x ->
    begin
      match Env.find_opt x @@ LexEnv.read () with
      | None ->
        Reporter.fatalf ?loc:node.loc ResolverError
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
      match Env.find_opt key @@ DynEnv.read () with
      | None ->
        Reporter.fatalf ?loc:node.loc ResolverError
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


(* Just take only one argument, I guess *)
and eval_tag loc tag =
  let rec parse_attrs tag attrs : Syn.t -> _=
    function
    | {value = Syn.Group (Squares, [{value = Text key; _}]); _} :: {value = Group (Braces, [{value = Text value; _}]); _} :: rest ->
      let attrs = Bwd.Snoc (attrs, (key, value)) in
      parse_attrs tag attrs rest
    | {value = Syn.Group (Braces, body); _} :: rest ->
      let attrs = Bwd.to_list attrs in
      Range.locate_opt loc (Sem.Tag (tag, attrs, eval body)) :: eval rest
    | rest ->
      let attrs = Bwd.to_list attrs in
      Range.locate_opt loc (Sem.Tag (tag, attrs, [])) :: eval rest
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
