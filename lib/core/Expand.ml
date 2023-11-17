open Prelude
open Resolver
open Bwd

module Set = Set.Make (String)
module UnitMap = Map.Make (String)

type part = Frontmatter | Body [@@deriving show]
type exports = P.data Trie.Untagged.t

module U = Algaeff.Reader.Make (struct type t = exports UnitMap.t end)
module Fm = Algaeff.State.Make (struct type t = Syn.frontmatter end)

module Part =
struct
  include Algaeff.State.Make (struct type t = part end)

  let protect kont =
    let part = get () in
    let x = kont () in
    set part;
    x
end

let only_frontmatter loc () =
  match Part.get () with
  | Frontmatter -> ()
  | Body ->
    Reporter.fatal ?loc Frontmatter_in_body
      "encountered frontmatter-only code in the body"

let rec expand : Code.t -> Syn.t =
  function
  | [] -> []

  | {value = Prim (p, x); loc} :: rest ->
    Part.set Body;
    {value = Syn.Prim (p, expand x); loc} :: expand rest

  | {value = Text x; loc} :: rest ->
    if not (String.trim x = "") then
      Part.set Body;
    {value = Syn.Text x; loc} :: expand rest

  | {value = Let (a, bs, def); loc} :: rest ->
    let singl =
      Part.protect @@ fun () ->
      Trie.Untagged.singleton (a, `Term (expand_lambda loc (bs, def)))
    in
    Resolver.Scope.section [] @@ fun _ ->
    Resolver.Scope.import_subtree ([], singl);
    expand rest

  | {value = Namespace (path, body); loc} :: rest ->
    only_frontmatter loc ();
    let mode, result =
      Scope.section path @@ fun () ->
      let x = expand body in
      Part.get (), x
    in
    Part.set mode;
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
    Part.set Body;
    let dest = expand dest in
    let title = Option.some @@ expand title in
    let link = Range.locate_opt loc1 @@ Syn.Link {dest; title} in
    link :: expand rest

  | {value = Group (Squares, [{value = Group (Squares, dest); _}]); loc} :: rest ->
    let dest = expand dest in
    let link = Range.locate_opt loc @@ Syn.Link {dest; title = None} in
    link :: expand rest

  | {value = Group (d, xs); loc} :: rest ->
    Part.set Body;
    let group = Range.locate_opt loc @@ Syn.Group (d, expand xs) in
    group :: expand rest

  | {value = Transclude addr; loc} :: rest ->
    Part.set Body;
    let transclusion = Range.locate_opt loc @@ Syn.Transclude addr in
    transclusion :: expand rest

  | {value = Query query; loc} :: rest ->
    Part.set Body;
    let query =
      Range.locate_opt loc @@
      Syn.Query (Query.map expand query)
    in
    query :: expand rest

  | {value = Embed_tex xs; loc} :: rest ->
    Part.set Body;
    let tex =
      Range.locate_opt loc @@
      Syn.Embed_tex {source = expand xs}
    in
    tex :: expand rest

  | {value = Block (xs, ys); loc} :: rest ->
    Part.set Body;
    let block =
      Range.locate_opt loc @@
      Syn.Block (expand xs, expand ys)
    in
    block :: expand rest

  | {value = Math (m, xs); loc} :: rest ->
    Part.set Body;
    let math = Range.locate_opt loc @@ Syn.Math (m, expand xs) in
    math :: expand rest

  | {value = Ident (path, methods); loc} :: rest ->
    Part.set Body;
    let rec loop acc  =
      function
      | [] -> acc
      | m :: ms ->
        loop [Range.locate_opt loc (Syn.Call (acc, m))] ms
    in
    loop (expand_ident loc path) methods @ expand rest

  | {value = Scope body; _} :: rest ->
    let body =
      Scope.section [] @@ fun () ->
      expand body
    in
    body @ expand rest

  | {value = Put (k, v); loc} :: rest ->
    Part.set Body;
    let k = expand_sym loc k in
    let v = expand v in
    [Range.locate_opt loc @@ Syn.Put (k, v, expand rest)]

  | {value = Default (k, v); loc} :: rest ->
    Part.set Body;
    let k = expand_sym loc k in
    let v = expand v in
    [Range.locate_opt loc @@ Syn.Default (k, v, expand rest)]

  | {value = Get k; loc} :: rest ->
    Part.set Body;
    let k = expand_sym loc k in
    Range.locate_opt loc (Syn.Get k) :: expand rest

  | {value = Object {self; methods}; loc} :: rest ->
    Part.set Body;
    let self, methods =
      Scope.section [] @@ fun () ->
      let sym = Symbol.fresh [] in
      let var = Range.locate_opt None @@ Syn.Var sym in
      self |> Option.iter (fun self -> Scope.import_subtree ([], Trie.Untagged.singleton (self, `Term [var])));
      sym, List.map expand_method methods
    in
    Range.locate_opt loc (Syn.Object {self; methods}) :: expand rest

  | {value = Patch {obj; self; methods}; loc} :: rest ->
    Part.set Body;
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
    Range.locate_opt loc patched :: expand rest

  | {value = Call (obj, method_name); loc} :: rest ->
    Part.set Body;
    Range.locate_opt loc (Syn.Call (expand obj, method_name)) :: expand rest

  | {value = If_tex (x, y); loc} :: rest ->
    Part.set Body;
    let x = expand x in
    let y = expand y in
    Range.locate_opt loc (Syn.If_tex (x, y)) :: expand rest

  | {value = Xml_tag (title, attrs, body); loc} :: rest ->
    Part.set Body;
    let attrs =
      attrs |> List.map @@ fun (k, v) ->
      k, expand v
    in
    let body = expand body in
    let xml = Syn.Xml_tag (title, attrs, body) in
    Range.locate_opt loc xml :: expand rest

  | {value = Import (vis, dep); loc} :: rest ->
    only_frontmatter loc ();
    let import = UnitMap.find dep @@ U.read () in
    begin
      match vis with
      | Public -> Resolver.Scope.include_subtree ([], import)
      | Private -> Resolver.Scope.import_subtree ([], import)
    end;
    expand rest

  | {value = Def (path, xs, body); loc} :: rest ->
    only_frontmatter loc ();
    let lam = Part.protect @@ fun () -> expand_lambda loc (xs, body) in
    Resolver.Scope.include_singleton (path, (`Term lam, ()));
    expand rest

  | {value = Alloc path; loc} :: rest ->
    only_frontmatter loc ();
    let symbol = Symbol.fresh path in
    Resolver.Scope.include_singleton (path, (`Sym symbol, ()));
    expand rest

  | {value = Title xs; loc} :: rest ->
    only_frontmatter loc ();
    begin
      Part.protect @@ fun () ->
      Fm.modify @@ fun fm ->
      {fm with title = Option.some @@ expand xs}
    end;
    Part.set Frontmatter;
    expand rest

  | {value = Author author; loc} :: rest ->
    only_frontmatter loc ();
    begin
      Fm.modify @@ fun fm ->
      {fm with authors = fm.authors @ [author]}
    end;
    Part.set Frontmatter;
    expand rest

  | {value = Tag tag; loc} :: rest ->
    only_frontmatter loc ();
    begin
      Fm.modify @@ fun fm ->
      {fm with tags = fm.tags @ [tag]}
    end;
    Part.set Frontmatter;
    expand rest

  | {value = Taxon taxon; loc} :: rest ->
    only_frontmatter loc ();
    begin
      Fm.modify @@ fun fm ->
      {fm with taxon = Some taxon}
    end;
    Part.set Frontmatter;
    expand rest

  | {value = Date str; loc} :: rest ->
    only_frontmatter loc ();
    let date = Date.parse str in
    begin
      Fm.modify @@ fun fm ->
      {fm with date = Some date}
    end;
    Part.set Frontmatter;
    expand rest

  | {value = Meta (k, v); loc} :: rest ->
    only_frontmatter loc ();
    begin
      Part.protect @@ fun () ->
      let v = expand v in
      Fm.modify @@ fun fm ->
      {fm with metas = fm.metas @ [k,v]}
    end;
    Part.set Frontmatter;
    expand rest

  | {value = TeX_package pkg; loc} :: rest ->
    only_frontmatter loc ();
    begin
      Fm.modify @@ fun fm ->
      {fm with tex_packages = fm.tex_packages @ [pkg]}
    end;
    Part.set Frontmatter;
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
  [Range.locate_opt loc @@ Syn.Lam (syms, expand body)]

and expand_ident loc path =
  match Scope.resolve path, path with
  | None, [name] ->
    [Range.locate_opt loc @@ Syn.Unresolved name]
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

let expand_doc (units : exports UnitMap.t) addr (doc : Code.doc) =
  Reporter.tracef "when expanding tree at address `%s`" addr @@ fun () ->
  let init = Syn.{addr; title = None; taxon = None; date = None; authors = []; tags = []; metas = []; tex_packages = []} in
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
  Part.run ~init:Frontmatter @@ fun () ->
  let tree = expand doc in
  let fm = Fm.get () in
  let exports = Resolver.Scope.get_export () in
  UnitMap.add addr exports units, (fm, tree)
