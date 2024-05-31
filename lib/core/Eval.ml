open Base
open Bwd
open Prelude

module Lex_env = Algaeff.Reader.Make (struct type t = Sem.t Env.t end)
module Dyn_env = Algaeff.Reader.Make (struct type t = Sem.t Env.t end)
module Heap = Algaeff.State.Make (struct type t = Sem.obj Env.t end)
module Emitted_trees = Algaeff.State.Make (struct type t = Sem.tree list end)
module Fm = Algaeff.State.Make (struct type t = Sem.frontmatter end)

let get_transclusion_opts () =
  let dynenv = Dyn_env.read () in
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
    let dest = User_addr (Sem.string_of_nodes @@ eval_textual [] dest) in
    {node with value = Sem.Link (dest, title, Identity)} :: eval rest

  | Ref dest ->
    let addr = User_addr (Sem.string_of_nodes @@ eval_textual [] dest) in
    {node with value = Sem.Ref addr} :: eval rest

  | Math (mmode, e) ->
    {node with value = Sem.Math (mmode, eval e)} :: eval rest

  | Prim (p, body) ->
    {node with value = Sem.Prim (p, eval_trim body)} :: eval rest

  | Xml_tag (name, attrs, body) ->
    let rec process attrs = match attrs with
      | [] -> []
      | (k,v) :: attrs ->
        let processed = process attrs in
        if List.mem_assoc k processed then begin
          Reporter.emitf ?loc:node.loc Duplicate_attribute
            "skipping duplicate XML attribute `%a`" pp_xml_resolved_qname k;
          processed
        end else
          (k, eval v) :: processed
    in
    {node with value = Sem.Xml_tag (name, process attrs, eval body)} :: eval rest

  | Unresolved name ->
    {node with value = Sem.Unresolved name} :: eval rest

  | Transclude addr ->
    let opts = get_transclusion_opts () in
    let addr = User_addr (Sem.string_of_nodes @@ eval_textual [] addr) in
    {node with value = Sem.Transclude (opts, addr)} :: eval rest

  | Subtree (addr, nodes) ->
    let addr =
      match addr with
      | Some addr -> User_addr addr
      | None -> Machine_addr (Oo.id (object end))
    in
    let opts = get_transclusion_opts () in
    let subtree = eval_tree_inner ~addr nodes in
    let fm = Fm.get () in
    let subtree = {subtree with fm = {subtree.fm with physical_parent = Some fm.addr; designated_parent = Some fm.addr}} in
    begin
      Emitted_trees.modify @@ fun trees ->
      subtree :: trees
    end;
    {node with value = Sem.Subtree (opts, subtree)} :: eval rest

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

  | Lam (xs, body) ->
    let rec loop xs rest =
      match xs, rest with
      | [], rest -> eval body, rest
      | x :: xs, Range.{value = Syn.Group (Braces, u); loc = loc'} :: rest ->
        Lex_env.scope (Env.add x (eval u)) @@ fun () ->
        loop xs rest
      | x :: xs, Range.{value = Syn.Verbatim str; loc = loc'} :: rest ->
        let verb = [Range.{value = Sem.Verbatim str; loc = loc'}] in
        Lex_env.scope (Env.add x verb) @@ fun () ->
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
      let env = Lex_env.read () in
      let add (name, body) =
        let super = Symbol.fresh [] in
        Sem.MethodTable.add name Sem.{body; self; super; env}
      in
      List.fold_right add methods Sem.MethodTable.empty
    in
    let sym = Symbol.fresh ["obj"] in
    Heap.modify @@ Env.add sym Sem.{prototype = None; methods = table};
    {node with value = Sem.Object sym} :: eval rest

  | Patch {obj; self; super; methods} ->
    begin
      match eval_strip obj with
      | [Range.{value = Sem.Object obj_ptr; _}] as obj ->
        let table =
          let env = Lex_env.read () in
          let add (name, body) =
            Sem.MethodTable.add name
              Sem.{body; self; super; env}
          in
          List.fold_right add methods Sem.MethodTable.empty
        in
        let sym = Symbol.fresh ["obj"] in
        Heap.modify @@ Env.add sym Sem.{prototype = Some obj_ptr; methods = table};
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
            Lex_env.scope (fun _ -> env) @@ fun () ->
            eval mthd.body
          | None ->
            match obj.prototype with
            | Some proto ->
              call_method @@ Env.find proto @@ Heap.get ()
            | None ->
              Reporter.fatalf ?loc:node.loc Type_error
                "tried to call unbound method `%s`" method_name
        in

        let result = call_method @@ Env.find sym @@ Heap.get () in
        result @ eval rest
      | xs ->
        Reporter.fatalf ?loc:node.loc Type_error
          "tried to call method `%s` on non-object: %a" method_name Sem.pp xs
    end

  | Var x ->
    begin
      match Env.find_opt x @@ Lex_env.read () with
      | None ->
        Reporter.fatalf ?loc:node.loc Resolution_error
          "could not find variable named %a"
          Symbol.pp x
      | Some v -> v @ eval rest
    end

  | Put (k, v, body) ->
    let body =
      Dyn_env.scope (Env.add k @@ eval v) @@ fun () ->
      eval body
    in
    body @ eval rest

  | Default (k, v, body) ->
    let body =
      let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval v) flenv in
      Dyn_env.scope upd @@ fun () ->
      eval body
    in
    body @ eval rest

  | Get key ->
    begin
      let env = Dyn_env.read () in
      match Env.find_opt key @@ Dyn_env.read () with
      | None ->
        Eio.traceln "getting %a from %a" Symbol.pp key (Env.pp Sem.pp) env;
        Reporter.fatalf ?loc:node.loc Resolution_error
          "could not find fluid binding named %a"
          Symbol.pp key
      | Some v -> v @ eval rest
    end

  | Verbatim str ->
    {node with value = Sem.Verbatim str} :: eval rest

  | Group _ | Text _ ->
    eval_textual [] @@ node :: rest

  | Title title ->
    let title = eval title in
    Fm.modify (fun fm -> {fm with title = Some title});
    eval rest

  | Parent addr ->
    Fm.modify (fun fm -> {fm with designated_parent = Some (User_addr addr)});
    eval rest

  | Meta (k, v) ->
    begin
      let v = eval v in
      Fm.modify @@ fun fm ->
      {fm with metas = fm.metas @ [k,v]}
    end;
    eval rest

  | Author author ->
    begin
      Fm.modify @@ fun fm ->
      {fm with authors = fm.authors @ [User_addr author]}
    end;
    eval rest

  | Contributor author ->
    begin
      Fm.modify @@ fun fm ->
      {fm with contributors = fm.contributors @ [User_addr author]}
    end;
    eval rest

  | Tag tag ->
    begin
      Fm.modify @@ fun fm ->
      {fm with tags = fm.tags @ [tag]}
    end;
    eval rest

  | Date date ->
    let date = Date.parse date in
    begin
      Fm.modify @@ fun fm ->
      {fm with dates = fm.dates @ [date]}
    end;
    eval rest

  | Number num ->
    begin
      Fm.modify @@ fun fm ->
      {fm with number = Some num}
    end;
    eval rest

  | Taxon taxon ->
    begin
      Fm.modify @@ fun fm ->
      {fm with taxon = Some taxon}
    end;
    eval rest

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


and eval_tree_inner ~addr (tree : Syn.tree) : Sem.tree =
  let outer_fm = Fm.get () in
  let fm =
    {(Sem.empty_frontmatter ~addr) with
     source_path = outer_fm.source_path;
     authors = outer_fm.authors;
     dates = outer_fm.dates}
  in
  Fm.run ~init:fm @@ fun () ->
  let body = eval tree in
  let fm = Fm.get () in
  let open Sem in
  {fm; body}


let eval_tree ~addr ~source_path (tree : Syn.tree) : Sem.tree * Sem.tree list =
  let fm = {(Sem.empty_frontmatter ~addr) with source_path} in
  Fm.run ~init:fm @@ fun () ->
  Emitted_trees.run ~init:[] @@ fun () ->
  Heap.run ~init:Env.empty @@ fun () ->
  Lex_env.run ~env:Env.empty @@ fun () ->
  Dyn_env.run ~env:Env.empty @@ fun () ->
  let tree = eval_tree_inner ~addr tree in
  let emitted = Emitted_trees.get () in
  tree, emitted
