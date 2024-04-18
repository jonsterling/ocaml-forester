open Prelude
open Resolver
open Bwd

module UnitMap = Map.Make (String)

type exports = P.data Trie.Untagged.t

module U = Algaeff.State.Make (struct type t = exports UnitMap.t end)

module Builtins =
struct
  let create path =
    let sym = Symbol.fresh path in
    sym, fun () ->
      Resolver.Scope.include_singleton (path, (Sym sym, ()))

  module Transclude =
  struct
    let title_sym, alloc_title = create ["transclude"; "title"]
    let taxon_sym, alloc_taxon = create ["transclude"; "taxon"]
    let expanded_sym, alloc_expanded = create ["transclude"; "expanded"]
    let show_heading_sym, alloc_show_heading = create ["transclude"; "heading"]
    let toc_sym, alloc_toc = create ["transclude"; "toc"]
    let numbered_sym, alloc_numbered = create ["transclude"; "numbered"]
    let show_metadata_sym, alloc_show_metadata = create ["transclude"; "metadata"]
  end
end

let rec expand : Code.t -> Syn.t =
  function
  | [] -> []

  | {value = Prim (p, x); loc} :: rest ->
    {value = Syn.Prim (p, expand x); loc} :: expand rest

  | {value = Text x; loc} :: rest ->
    {value = Syn.Text x; loc} :: expand rest

  | {value = Namespace (path, body); loc} :: rest ->
    let result =
      Scope.section path @@ fun () ->
      expand body
    in
    result @ expand rest

  | {value = Open path; loc} :: rest ->
    Scope.section [] @@ fun () ->
    begin
      Scope.modify_visible @@
      Resolver.Lang.union [
        Resolver.Lang.all;
        Resolver.Lang.renaming path []
      ]
    end;
    expand rest

  | {value = Group (Squares, title); loc = loc1} :: {value = Group (Parens, dest); loc = loc2} :: rest ->
    let dest = expand dest in
    let title = Option.some @@ expand title in
    let link = Syn.Link {dest; title} in
    {value = link; loc = loc1} :: expand rest

  | {value = Group (Squares, [{value = Group (Squares, dest); _}]); loc} :: rest ->
    let dest = expand dest in
    {value = Syn.Link {dest; title = None}; loc} :: expand rest

  | {value = Group (d, xs); loc} :: rest ->
    {value = Syn.Group (d, expand xs); loc} :: expand rest

  | {value = Transclude addr; loc} :: rest ->
    {value = Syn.Transclude addr; loc} :: expand rest

  | {value = Subtree (addr, nodes); loc} :: rest ->
    let subtree = expand_tree_inner @@ Code.{source_path = None; addr = addr; code = nodes} in
    {value = Syn.Subtree (addr, subtree); loc} :: expand rest

  | {value = Query query; loc} :: rest ->
    {value = Syn.Query (Query.map expand query); loc} :: expand rest

  | {value = Embed_tex (preamble, source); loc} :: rest ->
    {value = Syn.Embed_tex {preamble = expand preamble; source = expand source}; loc} :: expand rest

  | {value = Math (m, xs); loc} :: rest ->
    {value = Syn.Math (m, expand xs); loc} :: expand rest

  | {value = Ident (path, methods); loc} :: rest ->
    let rec loop acc  =
      function
      | [] -> acc
      | m :: ms ->
        loop [Range.{value = Syn.Call (acc, m); loc}] ms
    in
    loop (expand_ident loc path) methods @ expand rest

  | {value = Ref x; loc} :: rest ->
    {value = Syn.Ref (expand x); loc} :: expand rest

  | {value = Scope body; _} :: rest ->
    let body =
      Scope.section [] @@ fun () ->
      expand body
    in
    body @ expand rest

  | {value = Put (k, v); loc} :: rest ->
    let k = expand_sym loc k in
    let v = expand v in
    [{value = Syn.Put (k, v, expand rest); loc}]

  | {value = Default (k, v); loc} :: rest ->
    let k = expand_sym loc k in
    let v = expand v in
    [{value = Syn.Default (k, v, expand rest); loc}]

  | {value = Get k; loc} :: rest ->
    let k = expand_sym loc k in
    {value = Syn.Get k; loc} :: expand rest

  | {value = Object {self; methods}; loc} :: rest ->
    let self, methods =
      Scope.section [] @@ fun () ->
      let sym = Symbol.fresh [] in
      let var = Range.{value = Syn.Var sym; loc} in
      self |> Option.iter (fun self -> Scope.import_subtree ([], Trie.Untagged.singleton (self, Resolver.P.Term [var])));
      sym, List.map expand_method methods
    in
    {value = Syn.Object {self; methods}; loc} :: expand rest

  | {value = Patch {obj; self; methods}; loc} :: rest ->
    let self, super, methods =
      Scope.section [] @@ fun () ->
      let self_sym = Symbol.fresh [] in
      let super_sym = Symbol.fresh [] in
      let self_var = Range.locate_opt None @@ Syn.Var self_sym in
      let super_var = Range.locate_opt None @@ Syn.Var super_sym in
      self |> Option.iter begin fun self ->
        Scope.import_subtree ([], Trie.Untagged.singleton (self, Resolver.P.Term [self_var]));
        Scope.import_subtree ([], Trie.Untagged.singleton (self @ ["super"], Resolver.P.Term [super_var]));
      end;
      self_sym, super_sym, List.map expand_method methods
    in
    let patched = Syn.Patch {obj = expand obj; self; super; methods} in
    {value = patched; loc} :: expand rest

  | {value = Call (obj, method_name); loc} :: rest ->
    {value = Syn.Call (expand obj, method_name); loc} :: expand rest

  | {value = If_tex (x, y); loc} :: rest ->
    let x = expand x in
    let y = expand y in
    {value = Syn.If_tex (x, y); loc} :: expand rest

  | {value = Xml_tag (title, attrs, body); loc} :: rest ->
    let title = expand_xml_ident loc title in
    let attrs =
      attrs |> List.map @@ fun (k, v) ->
      expand_xml_ident loc k, expand v
    in
    let body = expand body in
    {value = Syn.Xml_tag (title, attrs, body); loc} :: expand rest

  | {value = Import (vis, dep); loc} :: rest ->
    let import = UnitMap.find_opt dep @@ U.get () in
    begin
      match import with
      | None ->
        Reporter.emitf ?loc:loc Tree_not_found "Could not find tree %s" dep;
        expand rest
      | Some tree -> begin
          match vis with
          | Public -> Resolver.Scope.include_subtree ([], tree)
          | Private -> Resolver.Scope.import_subtree ([], tree)
        end;
        expand rest
    end;


  | {value = Let (a, bs, def); loc} :: rest ->
    let singl = Trie.Untagged.singleton (a, Resolver.P.Term (expand_lambda loc (bs, def))) in
    Resolver.Scope.section [] @@ fun _ ->
    Resolver.Scope.import_subtree ([], singl);
    expand rest

  | {value = Def (path, xs, body); loc} :: rest ->
    let lam = expand_lambda loc (xs, body) in
    Resolver.Scope.include_singleton (path, (Term lam, ()));
    expand rest

  | {value = Decl_xmlns (prefix, xmlns); loc} :: rest ->
    let path = ["xmlns"; prefix] in
    let singl = Trie.Untagged.singleton (path, Resolver.P.Xmlns {prefix; xmlns}) in
    Resolver.Scope.include_singleton (path, (Xmlns {prefix; xmlns}, ()));
    expand rest

  | {value = Alloc path; loc} :: rest ->
    let symbol = Symbol.fresh path in
    Resolver.Scope.include_singleton (path, (Sym symbol, ()));
    expand rest

  | {value = Title xs; loc} :: rest ->
    {value = Syn.Title (expand xs); loc} :: expand rest

  | {value = Parent addr; loc} :: rest ->
    {value = Syn.Parent addr; loc} :: expand rest

  | {value = Author author; loc} :: rest ->
    {value = Syn.Author author; loc} :: expand rest

  | {value = Contributor author; loc} :: rest ->
    {value = Syn.Contributor author; loc} :: expand rest

  | {value = Tag tag; loc} :: rest ->
    {value = Syn.Tag tag; loc} :: expand rest

  | {value = Taxon taxon; loc} :: rest ->
    {value = Syn.Taxon taxon; loc} :: expand rest

  | {value = Date str; loc} :: rest ->
    {value = Syn.Date str; loc} :: expand rest

  | {value = Number str; loc} :: rest ->
    {value = Syn.Number str; loc} :: expand rest

  | {value = Meta (k, v); loc} :: rest ->
    {value = Syn.Meta (k, expand v); loc} :: expand rest

and expand_method (key, body) =
  key, expand body

and expand_lambda loc : Trie.path list * Code.t -> Syn.t =
  fun (xs, body) ->
  Scope.section [] @@ fun () ->
  let syms =
    xs |> List.map @@ fun x ->
    let sym = Symbol.fresh x in
    let var = Range.locate_opt None @@ Syn.Var sym in
    let singlx = Trie.Untagged.singleton (x, Resolver.P.Term [var]) in
    Scope.import_subtree ([], singlx);
    sym
  in
  [Range.{value = Syn.Lam (syms, expand body); loc}]

and expand_ident loc path =
  match Scope.resolve path, path with
  | None, [name] ->
    [Range.{value = Syn.Unresolved name; loc}]
  | None, _ ->
    Reporter.fatalf ?loc Resolution_error
      "path %a could not be resolved"
      Trie.pp_path path
  | Some (Term x, ()), _ ->
    let relocate Range.{value; _} = Range.{value; loc} in
    List.map relocate x
  | Some (Sym x, ()), _ ->
    Reporter.fatalf ?loc Resolution_error
      "path %a resolved to symbol %a instead of term"
      Trie.pp_path path
      Symbol.pp x
  | Some (Xmlns {xmlns; prefix}, ()), _ ->
    Reporter.fatalf ?loc Resolution_error
      "path %a resolved to xmlns:%s=\"%s\" instead of term"
      Trie.pp_path path
      xmlns
      prefix
  | Some (Tree_set addrs, ()), _ ->
    Reporter.fatalf ?loc Resolution_error
      "path %a resolved to tree set instead of term"
      Trie.pp_path path

and expand_sym loc path =
  match Scope.resolve path, path with
  | Some (Sym x, ()), _ -> x
  | _ ->
    Reporter.fatalf ?loc Resolution_error
      "expected path `%a` to resolve to symbol"
      Trie.pp_path path

and expand_xml_ident loc (prefix, uname) =
  match prefix with
  | None -> {xmlns = None; prefix = None; uname}
  | Some prefix ->
    match Scope.resolve ["xmlns"; prefix] with
    | Some (Xmlns {xmlns; prefix}, ()) ->
      {xmlns = Some xmlns; prefix = Some prefix; uname}
    | _ ->
      Reporter.fatalf ?loc Resolution_error
        "expected path `%s` to resolve to xmlns"
        prefix

and expand_tree_inner (tree : Code.tree) : Syn.tree =
  let trace f =
    match tree.addr with
    | Some addr -> Reporter.tracef "when expanding tree at address `%s`" addr f
    | None -> f ()
  in

  trace @@ fun () ->
  Scope.section [] @@ fun () ->
  let units = U.get () in
  let syn = expand tree.code in
  let exports = Resolver.Scope.get_export () in
  let units =
    match tree.addr with
    | None -> units
    | Some addr -> UnitMap.add addr exports units
  in
  U.set units;
  syn


let expand_tree (units : exports UnitMap.t) (tree : Code.tree) =
  U.run ~init:units @@ fun () ->
  Resolver.Scope.run @@ fun () ->
  Builtins.Transclude.alloc_title ();
  Builtins.Transclude.alloc_taxon ();
  Builtins.Transclude.alloc_expanded ();
  Builtins.Transclude.alloc_show_heading ();
  Builtins.Transclude.alloc_toc ();
  Builtins.Transclude.alloc_numbered ();
  Builtins.Transclude.alloc_show_metadata ();

  let tree = expand_tree_inner tree in
  let units = U.get () in
  units, tree
