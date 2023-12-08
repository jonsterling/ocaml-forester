open Prelude
open Resolver
open Bwd

module Set = Set.Make (String)
module UnitMap = Map.Make (String)

type exports = P.data Trie.Untagged.t

module U = Algaeff.Reader.Make (struct type t = exports UnitMap.t end)
module Fm = Algaeff.State.Make (struct type t = Syn.frontmatter end)

let rec expand : Code.t -> Syn.t =
  function
  | [] -> []

  | {value = Prim (p, x); loc} :: rest ->
    {value = Syn.Prim (p, expand x); loc} :: expand rest

  | {value = Text x; loc} :: rest ->
    {value = Syn.Text x; loc} :: expand rest

  | {value = Let (a, bs, def); loc} :: rest ->
    let singl = Trie.Untagged.singleton (a, `Term (expand_lambda loc (bs, def))) in
    Resolver.Scope.section [] @@ fun _ ->
    Resolver.Scope.import_subtree ([], singl);
    expand rest

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

  | {value = Query query; loc} :: rest ->
    {value = Syn.Query (Query.map expand query); loc} :: expand rest

  | {value = Embed_tex xs; loc} :: rest ->
    {value = Syn.Embed_tex {source = expand xs}; loc} :: expand rest

  | {value = Block (xs, ys); loc} :: rest ->
    {value = Syn.Block (expand xs, expand ys); loc} :: expand rest

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
      self |> Option.iter (fun self -> Scope.import_subtree ([], Trie.Untagged.singleton (self, `Term [var])));
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
        Scope.import_subtree ([], Trie.Untagged.singleton (self, `Term [self_var]));
        Scope.import_subtree ([], Trie.Untagged.singleton (self @ ["super"], `Term [super_var]));
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
    let attrs =
      attrs |> List.map @@ fun (k, v) ->
      k, expand v
    in
    let body = expand body in
    {value = Syn.Xml_tag (title, attrs, body); loc} :: expand rest

  | {value = Import (vis, dep); loc} :: rest ->
    let import = UnitMap.find dep @@ U.read () in
    begin
      match vis with
      | Public -> Resolver.Scope.include_subtree ([], import)
      | Private -> Resolver.Scope.import_subtree ([], import)
    end;
    expand rest

  | {value = Def (path, xs, body); loc} :: rest ->
    let lam = expand_lambda loc (xs, body) in
    Resolver.Scope.include_singleton (path, (`Term lam, ()));
    expand rest

  | {value = Alloc path; loc} :: rest ->
    let symbol = Symbol.fresh path in
    Resolver.Scope.include_singleton (path, (`Sym symbol, ()));
    expand rest

  | {value = Title xs; loc} :: rest ->
    begin
      Fm.modify @@ fun fm ->
      {fm with title = Option.some @@ expand xs}
    end;
    expand rest

  | {value = Author author; loc} :: rest ->
    begin
      Fm.modify @@ fun fm ->
      {fm with authors = fm.authors @ [author]}
    end;
    expand rest

  | {value = Tag tag; loc} :: rest ->
    begin
      Fm.modify @@ fun fm ->
      {fm with tags = fm.tags @ [tag]}
    end;
    expand rest

  | {value = Taxon taxon; loc} :: rest ->
    begin
      Fm.modify @@ fun fm ->
      {fm with taxon = Some taxon}
    end;
    expand rest

  | {value = Date str; loc} :: rest ->
    let date = Date.parse str in
    begin
      Fm.modify @@ fun fm ->
      {fm with dates = fm.dates @ [date]}
    end;
    expand rest

  | {value = Meta (k, v); loc} :: rest ->
    begin
      let v = expand v in
      Fm.modify @@ fun fm ->
      {fm with metas = fm.metas @ [k,v]}
    end;
    expand rest

  | {value = TeX_package pkg; loc} :: rest ->
    begin
      Fm.modify @@ fun fm ->
      {fm with tex_packages = fm.tex_packages @ [pkg]}
    end;
    expand rest

and expand_method (key, body) =
  key, expand body

and expand_lambda loc : Trie.path list * Code.t -> Syn.t =
  fun (xs, body) ->
  Scope.section [] @@ fun () ->
  let syms =
    xs |> List.map @@ fun x ->
    let sym = Symbol.fresh x in
    let var = Range.locate_opt None @@ Syn.Var sym in
    let singlx = Trie.Untagged.singleton (x, `Term [var]) in
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
  | Some (`Term x, ()), _ ->
    let relocate Range.{value; _} = Range.{value; loc} in
    List.map relocate x
  | Some (`Sym x, ()), _ ->
    Reporter.fatalf ?loc Resolution_error
      "path %a resolved to symbol %a instead of term"
      Trie.pp_path path
      Symbol.pp x

and expand_sym loc path =
  match Scope.resolve path, path with
  | Some (`Sym x, ()), _ -> x
  | _ ->
    Reporter.fatalf ?loc Resolution_error
      "expected path `%a` to resolve to symbol"
      Trie.pp_path path

module Builtins =
struct
  let create path =
    let sym = Symbol.fresh path in
    sym, fun () ->
      Resolver.Scope.include_singleton (path, (`Sym sym, ()))

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

let expand_tree (units : exports UnitMap.t) (tree : Code.tree) =
  Reporter.tracef "when expanding tree at address `%s`" tree.addr @@ fun () ->
  let init = Syn.{addr = tree.addr; title = None; taxon = None; dates = []; authors = []; tags = []; metas = []; tex_packages = []; source_path = tree.source_path} in
  Resolver.Scope.run @@ fun () ->
  Builtins.Transclude.alloc_title ();
  Builtins.Transclude.alloc_taxon ();
  Builtins.Transclude.alloc_expanded ();
  Builtins.Transclude.alloc_show_heading ();
  Builtins.Transclude.alloc_toc ();
  Builtins.Transclude.alloc_numbered ();
  Builtins.Transclude.alloc_show_metadata ();

  U.run ~env:units @@ fun () ->
  Fm.run ~init @@ fun () ->
  let syn = expand tree.code in
  let fm = Fm.get () in
  let exports = Resolver.Scope.get_export () in
  UnitMap.add tree.addr exports units, (fm, syn)
