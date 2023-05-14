module Y = Yuujinchou
open Resolver

module Set = Set.Make (String)
module UnitMap = Map.Make (String)

type exports = Term.t Trie.Untagged.t

let rec expand (fm : Code.frontmatter) (env : Term.t Env.t) : Code.t -> Term.t =
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
    let env' = Env.add a (expand_lambda fm env (bs, xs)) env in 
    expand fm env' rest
  | Block (xs, ys) :: rest -> 
    Block (expand fm env xs, expand fm env ys) :: expand fm env rest 
  | Math (m, xs) :: rest ->
    Math (m, expand fm env xs) :: expand fm env rest 
  | Ident str :: rest -> 
    expand_ident env str @ expand fm env rest

and expand_ident env str = 
  match Env.find_opt str env with 
  | Some x -> x
  | None -> 
    match Scope.resolve [str] with 
    | None -> 
      [Tag str]
    | Some (x, ()) -> x

and expand_lambda fm (env : Term.t Env.t) : Code.macro -> Term.t = 
  fun (xs, body) -> 
  let env' = List.fold_left (fun env x -> Env.add x [Term.Var x] env) env xs in
  [Term.Lam (xs, expand fm env' body)]


let expand_doc units addr (doc : Code.doc) =
  let fm, tree = doc in
  Resolver.Scope.run @@ fun () ->
  begin
    fm.decls |> List.iter @@ function
    | Code.Import dep -> 
      let import = UnitMap.find dep units in
      Resolver.Scope.import_subtree ([], import)
    | Export dep -> 
      let import = UnitMap.find dep units in
      Resolver.Scope.include_subtree ([], import)
    | Code.Def (path, ((xs,body) as macro)) ->
      let macro = expand_lambda fm Env.empty macro in
      Resolver.Scope.include_singleton (path, (macro, ()));
      Resolver.Scope.export_visible (Y.Language.only path)
  end;

  let exports = Resolver.Scope.get_export () in
  let units = UnitMap.add addr exports units in
  let tree = expand fm Env.empty tree in
  let title = fm.title |> Option.map @@ expand fm Env.empty in
  let metas = 
    fm.metas |> List.map @@ fun (key, body) ->
    key, expand fm Env.empty body
  in

  let fm = Term.{title; addr; taxon = fm.taxon; authors = fm.authors; date = fm.date; tags = fm.tags; metas; tex_packages = fm.tex_packages} in
  units, (fm, tree)
