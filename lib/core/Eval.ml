open Base
open Bwd
open Prelude

module LexEnv = Algaeff.Reader.Make (struct type t = Sem.t Env.t end)
module DynEnv = Algaeff.Reader.Make (struct type t = Sem.t Env.t end)
module HeapState = Algaeff.State.Make (struct type t = Sem.obj Env.t end)
module EmittedTrees = Algaeff.State.Make (struct type t = Sem.tree list end)

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
    {node with value = Sem.Link {dest; title; modifier = None}} :: eval rest
  | Ref dest ->
    let addr = Sem.string_of_nodes @@ eval_textual [] dest in
    {node with value = Sem.Ref {addr}} :: eval rest
  | Math (mmode, e) ->
    {node with value = Sem.Math (mmode, eval e)} :: eval rest
  | Prim (p, body) ->
    {node with value = Sem.Prim (p, eval_trim body)} :: eval rest
  | Xml_tag (name, attrs, body) ->
    let attrs =
      attrs |> List.map @@ fun (k, v) ->
      k, eval v
    in
    {node with value = Sem.Xml_tag (name, attrs, eval body)} :: eval rest
  | Unresolved name ->
    {node with value = Sem.Unresolved name} :: eval rest
  | Transclude addr ->
    let opts = get_transclusion_opts () in
    {node with value = Sem.Transclude (opts, addr)} :: eval rest
  | Subtree (fm, nodes) ->
    let opts = get_transclusion_opts () in
    let addr =
      match fm.addr with
      | None ->
        Format.sprintf "anon.%i" (Oo.id (object end))
      | Some addr -> addr
    in
    let fm = {fm with addr = Some addr} in
    let subtree = eval_tree_inner (fm, nodes) in
    EmittedTrees.modify (fun trees -> subtree :: trees);
    {node with value = Sem.Transclude (opts, addr)} :: eval rest
  | If_tex (x , y) ->
    let x = eval x in
    let y = eval y in
    {node with value = Sem.If_tex (x, y)} :: eval rest
  | Query query ->
    let opts = get_transclusion_opts () in
    let opts =
      match opts.title_override with
      | None -> {opts with show_heading = false; toc = false}
      | Some _ -> opts
    in
    let query = Query.map eval query in
    {node with value = Sem.Query (opts, query)} :: eval rest
  | Embed_tex {preamble; source} ->
    {node with value = Sem.Embed_tex {preamble = eval preamble; source = eval source}} :: eval rest
  | Block (title, body) ->
    {node with value = Sem.Block (eval title, eval body)} :: eval rest
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
  | Object {self; methods} ->
    let table =
      let env = LexEnv.read () in
      let add (name, body) =
        let super = Symbol.fresh [] in
        Sem.MethodTable.add name Sem.{body; self; super; env}
      in
      List.fold_right add methods Sem.MethodTable.empty
    in
    let sym = Symbol.fresh ["obj"] in
    HeapState.modify @@ Env.add sym Sem.{prototype = None; methods = table};
    {node with value = Sem.Object sym} :: eval rest
  | Patch {obj; self; super; methods} ->
    begin
      match eval_strip obj with
      | [Range.{value = Sem.Object obj_ptr; _}] as obj ->
        let table =
          let env = LexEnv.read () in
          let add (name, body) =
            Sem.MethodTable.add name
              Sem.{body; self; super; env}
          in
          List.fold_right add methods Sem.MethodTable.empty
        in
        let sym = Symbol.fresh ["obj"] in
        HeapState.modify @@ Env.add sym Sem.{prototype = Some obj_ptr; methods = table};
        {node with value = Sem.Object sym} :: eval rest
      | xs ->
        Reporter.fatalf ?loc:node.loc Type_error
          "tried to patch non-object"
    end
  | Call (obj, method_name) ->
    begin
      match eval_strip obj with
      | [Range.{value = Sem.Object sym; _}] as obj_val ->
        let rec call_method (obj : Sem.obj) =
          let proto_val =
            match obj.prototype with
            | None -> None
            | Some ptr -> Some [Range.locate_opt None @@ Sem.Object ptr]
          in
          match Sem.MethodTable.find_opt method_name obj.methods with
          | Some mthd ->
            let env =
              let env = Env.add mthd.self obj_val mthd.env in
              match proto_val with
              | None -> env
              | Some proto_val ->
                Env.add mthd.super proto_val env
            in
            LexEnv.scope (fun _ -> env) @@ fun () ->
            eval mthd.body
          | None ->
            match obj.prototype with
            | Some proto ->
              call_method @@ Env.find proto @@ HeapState.get ()
            | None ->
              Reporter.fatalf ?loc:node.loc Type_error
                "tried to call unbound method `%s`" method_name
        in

        let result = call_method @@ Env.find sym @@ HeapState.get () in
        result @ eval rest
      | xs ->
        Reporter.fatalf ?loc:node.loc Type_error
          "tried to call method `%s` on non-object: %a" method_name Sem.pp xs
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

and eval_strip xs = Sem.strip_whitespace @@ eval xs

and eval_trim xs = Sem.trim_whitespace @@ eval xs

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


and eval_tree_inner (tree : Syn.tree) : Sem.tree =
  let fm, tree = tree in
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
   contributors = fm.contributors;
   dates = fm.dates;
   tags = fm.tags;
   source_path = fm.source_path;
   metas}


let eval_tree (tree : Syn.tree) : Sem.tree * Sem.tree list =
  let fm = fst tree in
  EmittedTrees.run ~init:[] @@ fun () ->
  HeapState.run ~init:Env.empty @@ fun () ->
  LexEnv.run ~env:Env.empty @@ fun () ->
  DynEnv.run ~env:Env.empty @@ fun () ->
  let tree = eval_tree_inner tree in
  let emitted = EmittedTrees.get () in
  tree, emitted
