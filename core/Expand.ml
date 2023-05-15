open Prelude
open Resolver

module Set = Set.Make (String)
module UnitMap = Map.Make (String)

type exports = P.data Trie.Untagged.t

let rec expand (fm : Syn.frontmatter) (env : Syn.t Trie.Untagged.t) : Code.t -> Syn.t =
  function
  | [] -> []
  | Text x :: rest ->
    Text x :: expand fm env rest
  | Group (Squares, title) :: Group (Parens, [Text dest]) :: rest ->
    let title = expand fm env title in
    Link {dest; title} :: expand fm env rest
  | Group (d, xs) :: rest ->
    Group (d, expand fm env xs) :: expand fm env rest
  | Transclude (m, addr) :: rest ->
    Transclude (m, addr) :: expand fm env rest
  | EmbedTeX xs :: rest ->
    EmbedTeX {packages = fm.tex_packages; source = expand fm env xs} :: expand fm env rest
  | Let (a, bs, xs) :: rest ->
    let env' = 
      env |>  Trie.Untagged.update_singleton a @@ fun _ -> 
      Option.some @@ expand_lambda fm env (bs, xs)
    in
    expand fm env' rest
  | Block (xs, ys) :: rest ->
    Block (expand fm env xs, expand fm env ys) :: expand fm env rest
  | Math (m, xs) :: rest ->
    Math (m, expand fm env xs) :: expand fm env rest
  | Ident str :: rest ->
    expand_ident env str @ expand fm env rest
  | Scope body :: rest ->
    let body = expand fm env body in 
    body @ expand fm env rest
  | Put (k, v) :: rest ->
    let k = expand_sym k in
    let v = expand fm env v in
    [Put (k, v, expand fm env rest)]
  | Default (k, v) :: rest -> 
    let k = expand_sym k in
    let v = expand fm env v in
    [Default (k, v, expand fm env rest)]
  | Get k :: rest -> 
    let k = expand_sym k in 
    Get k :: expand fm env rest

and expand_ident env path =
  match Trie.Untagged.find_singleton path env with
  | Some x -> x
  | _ ->
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



and expand_lambda fm (env : Syn.t Trie.Untagged.t) : Code.binder -> Syn.t =
  fun (xs, body) ->
  let set env x =
    env |> Trie.Untagged.update_singleton [x] @@ fun _ -> 
    Some [Syn.Var x]
  in
  let env' = List.fold_left set env xs in
  [Syn.Lam (xs, expand fm env' body)]

let rec expand_decls units =
  List.fold_left @@ expand_decl units

and expand_decl units fm = 
  function 
  | Code.Import (vis, dep) -> 
    let import = UnitMap.find dep units in
    begin
      match vis with
      | Public -> Resolver.Scope.include_subtree ([], import)
      | Private -> Resolver.Scope.import_subtree ([], import)
    end;
    fm
  | Code.Def (path, xs, body) ->
    let lam = expand_lambda fm Trie.Untagged.empty (xs, body) in
    Resolver.Scope.include_singleton (path, (`Term lam, ()));
    fm
  | Code.Alloc path ->
    let symbol = Symbol.fresh path in 
    Resolver.Scope.include_singleton (path, (`Sym symbol, ())); 
    fm
  | Code.Title xs ->
    {fm with title = Option.some @@ expand fm Trie.empty xs}
  | Code.Author author ->
    {fm with authors = fm.authors @ [author]}
  | Code.Tag tag -> 
    {fm with tags = fm.tags @ [tag]}
  | Code.Taxon taxon -> 
    {fm with taxon = Some taxon}
  | Code.Date str -> 
    let date = Date.parse str in
    {fm with date = Some date}
  | Code.Meta (k, v) -> 
    let v = expand fm Trie.empty v in
    {fm with metas = fm.metas @ [k,v]}
  | Code.TeXPackage pkg -> 
    {fm with tex_packages = fm.tex_packages @ [pkg]}
  | Code.Namespace (path, body) ->
    Scope.section path @@ fun () ->
    expand_decls units fm body

let expand_doc (units : exports UnitMap.t) addr (doc : Code.doc) = 
  let fm, tree = doc in 
  let init = Syn.{addr; title = None; taxon = None; date = None; authors = []; tags = []; metas = []; tex_packages = []} in
  Resolver.Scope.run @@ fun () ->
  let fm = expand_decls units init fm in
  let exports = Resolver.Scope.get_export () in
  let units = UnitMap.add addr exports units in
  let tree = expand fm Trie.empty tree in
  units, (fm, tree)