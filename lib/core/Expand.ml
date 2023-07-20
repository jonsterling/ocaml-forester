open Prelude
open Resolver

module Set = Set.Make (String)
module UnitMap = Map.Make (String)

type mode = Frontmatter | Body [@@deriving show]
type exports = P.data Trie.Untagged.t

module U = Algaeff.Reader.Make (struct type env = exports UnitMap.t end)
module Fm = Algaeff.State.Make (struct type state = Syn.frontmatter end)
module Mode = 
struct 
  include Algaeff.State.Make (struct type state = mode end)

  let protect kont = 
    let mode = get () in 
    let x = kont () in
    set mode;
    x
end

exception FrontmatterInBody of Code.t

let only_frontmatter code () = 
  match Mode.get () with 
  | Frontmatter -> () 
  | Body -> raise @@ FrontmatterInBody code

let rec expand (code : Code.t) : Syn.t = 
  match code with
  | [] -> []
  | Text x :: rest -> 
    if not (String.trim x = "") then 
      Mode.set Body;
    Syn.Text x :: expand rest
  | Let (a, bs, def) :: rest ->
    let singl = 
      Mode.protect @@ fun () -> 
      Trie.Untagged.singleton (a, `Term (expand_lambda (bs, def))) 
    in
    Resolver.Scope.section [] @@ fun _ -> 
    Resolver.Scope.import_subtree ([], singl);
    expand rest
  | Namespace (path, body) :: rest -> 
    only_frontmatter code ();
    let mode, result =
      Scope.section path @@ fun () -> 
      let x = expand body in 
      Mode.get (), x
    in
    Mode.set mode;
    result @ expand rest
  | Open path :: rest -> 
    Scope.section [] @@ fun () -> 
    Scope.modify_visible @@ 
    Resolver.Lang.union [
      Resolver.Lang.all;
      Resolver.Lang.renaming path []
    ];
    expand rest 
  | Group (Squares, title) :: Group (Parens, [Text dest]) :: rest ->
    Mode.set Body;
    let title = expand title in
    Syn.Link {dest; title} :: expand rest
  | Group (d, xs) :: rest ->
    Mode.set Body;
    Syn.Group (d, expand xs) :: expand rest
  | Transclude addr :: rest ->
    Mode.set Body;
    Syn.Transclude addr :: expand rest
  | Query query :: rest -> 
    Mode.set Body;
    let query = Query.map expand query in
    Syn.Query query :: expand rest
  | EmbedTeX xs :: rest ->
    Mode.set Body;
    let fm = Fm.get () in
    Syn.EmbedTeX {packages = fm.tex_packages; source = expand xs} :: expand rest
  | Block (xs, ys) :: rest ->
    Mode.set Body;
    Syn.Block (expand xs, expand ys) :: expand rest
  | Math (m, xs) :: rest ->
    Mode.set Body;
    Syn.Math (m, expand xs) :: expand rest
  | Ident str :: rest ->
    Mode.set Body;
    expand_ident str @ expand rest
  | Scope body :: rest ->
    let body = 
      Scope.section [] @@ fun () -> 
      expand body
    in
    body @ expand rest
  | Put (k, v) :: rest ->
    Mode.set Body;
    let k = expand_sym k in
    let v = expand v in
    [Syn.Put (k, v, expand rest)]
  | Default (k, v) :: rest ->
    Mode.set Body;
    let k = expand_sym k in
    let v = expand v in
    [Syn.Default (k, v, expand rest)]
  | Get k :: rest -> 
    Mode.set Body;
    let k = expand_sym k in 
    Syn.Get k :: expand rest
  | Import (vis, dep) :: rest -> 
    only_frontmatter code ();
    let import = UnitMap.find dep @@ U.read () in
    begin
      match vis with
      | Public -> Resolver.Scope.include_subtree ([], import)
      | Private -> Resolver.Scope.import_subtree ([], import)
    end;
    expand rest
  | Def (path, xs, body) :: rest ->
    only_frontmatter code ();
    let lam = Mode.protect @@ fun () -> expand_lambda (xs, body) in
    Resolver.Scope.include_singleton (path, (`Term lam, ()));
    expand rest
  | Alloc path :: rest ->
    only_frontmatter code ();
    let symbol = Symbol.fresh path in 
    Resolver.Scope.include_singleton (path, (`Sym symbol, ())); 
    expand rest
  | Title xs :: rest ->
    only_frontmatter code ();
    begin 
      Mode.protect @@ fun () -> 
      Fm.modify @@ fun fm -> 
      {fm with title = Option.some @@ expand xs}
    end;
    Mode.set Frontmatter;
    expand rest
  | Author author :: rest ->
    only_frontmatter code ();
    begin
      Fm.modify @@ fun fm ->
      {fm with authors = fm.authors @ [author]}
    end;
    Mode.set Frontmatter;
    expand rest
  | Tag tag :: rest -> 
    only_frontmatter code ();
    begin 
      Fm.modify @@ fun fm -> 
      {fm with tags = fm.tags @ [tag]}
    end;
    Mode.set Frontmatter;
    expand rest 
  | Taxon taxon :: rest -> 
    only_frontmatter code ();
    begin 
      Fm.modify @@ fun fm -> 
      {fm with taxon = Some taxon}
    end;
    Mode.set Frontmatter;
    expand rest
  | Date str :: rest -> 
    only_frontmatter code ();
    let date = Date.parse str in
    begin 
      Fm.modify @@ fun fm -> 
      {fm with date = Some date}
    end;
    Mode.set Frontmatter;
    expand rest
  | Meta (k, v) :: rest -> 
    only_frontmatter code ();
    begin
      Mode.protect @@ fun () ->
      let v = expand v in
      Fm.modify @@ fun fm ->
      {fm with metas = fm.metas @ [k,v]}
    end; 
    Mode.set Frontmatter;
    expand rest    
  | TeXPackage pkg :: rest -> 
    only_frontmatter code ();
    begin 
      Fm.modify @@ fun fm -> 
      {fm with tex_packages = fm.tex_packages @ [pkg]}
    end;
    Mode.set Frontmatter;
    expand rest


and expand_lambda : Trie.path list * Code.t -> Syn.t =
  fun (xs, body) ->
  Scope.section [] @@ fun () -> 
  let syms = 
    xs |> List.map @@ fun x -> 
    let sym = Symbol.fresh x in 
    let singlx = Trie.Untagged.singleton (x, `Term [Syn.Var sym]) in
    Scope.import_subtree ([], singlx);
    sym
  in
  [Syn.Lam (syms, expand body)]

and expand_ident path =
  match Scope.resolve path, path with
  | None, [name] ->
    [Tag name]
  | Some (`Term x, ()), _ -> x
  | _ -> 
    failwith "expand_ident"

and expand_sym path =
  match Scope.resolve path, path with
  | None, [name] ->
    failwith "expand_sym"
  | Some (`Sym x, ()), _ -> x
  | _ -> 
    failwith "expand_ident"


module Builtins = 
struct
  let create path =
    let sym = Symbol.fresh path in 
    sym, fun () -> 
      Resolver.Scope.include_singleton (path, (`Sym sym, ()))

  module Transclude = 
  struct 
    let title_sym, alloc_title = create ["transclude"; "title"]
    let expanded_sym, alloc_expanded = create ["transclude"; "expanded"]
    let show_heading_sym, alloc_show_heading = create ["transclude"; "heading"]
    let toc_sym, alloc_toc = create ["transclude"; "toc"]
    let numbered_sym, alloc_numbered = create ["transclude"; "numbered"]
    let show_metadata_sym, alloc_show_metadata = create ["transclude"; "metadata"]
  end
end 

let expand_doc (units : exports UnitMap.t) addr (doc : Code.doc) = 
  let init = Syn.{addr; title = None; taxon = None; date = None; authors = []; tags = []; metas = []; tex_packages = []} in
  Resolver.Scope.run @@ fun () ->
  Builtins.Transclude.alloc_title ();
  Builtins.Transclude.alloc_expanded ();
  Builtins.Transclude.alloc_show_heading ();
  Builtins.Transclude.alloc_toc ();
  Builtins.Transclude.alloc_numbered ();
  Builtins.Transclude.alloc_show_metadata ();

  U.run ~env:units @@ fun () ->
  Fm.run ~init @@ fun () -> 
  Mode.run ~init:Frontmatter @@ fun () ->
  try 
    let tree = expand doc in
    let fm = Fm.get () in
    let exports = Resolver.Scope.get_export () in
    UnitMap.add addr exports units, (fm, tree)
  with 
  | FrontmatterInBody code as exn -> 
    Format.eprintf "[%s] Encountered frontmatter-only code in body: %a@." addr Code.pp code;
    raise exn 
