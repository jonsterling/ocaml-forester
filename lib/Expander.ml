open Types
open Resolver

module Set = Set.Make (String)

let rec expand (env : Term.t Env.t) : Code.t -> Term.t =
  function 
  | [] -> []
  | Text x :: rest -> 
    Text x :: expand env rest
  | Group (Squares, title) :: Group (Parens, [Text dest]) :: rest ->
    let title = expand env title in 
    Link {dest; title} :: expand env rest
  | Group (d, xs) :: rest -> 
    Group (d, expand env xs) :: expand env rest
  | Transclude (m, addr) :: rest ->
    Transclude (m, addr) :: expand env rest
  | EmbedTeX xs :: rest -> 
    EmbedTeX (expand env xs) :: expand env rest
  | Let (a, bs, xs) :: rest as all -> 
    let env' = Env.add a (expand_macro env (bs, xs)) env in 
    expand env' rest
  | Block (xs, ys) :: rest -> 
    Block (expand env xs, expand env ys) :: expand env rest 
  | Math (m, xs) :: rest ->
    Math (m, expand env xs) :: expand env rest 
  | Ident str :: rest as all -> 
    expand_ident env str @ expand env rest 

and expand_ident env str = 
  match Env.find_opt str env with 
  | Some x -> x
  | None -> 
    match Scope.resolve [str] with 
    | None -> 
      [Tag str]
    | Some (x, ()) -> x

and expand_macro (env : Term.t Env.t) : Code.macro -> Term.t = 
  fun (xs, body) -> 
  let env' = List.fold_left (fun env x -> Env.add x [Term.Var x] env) env xs in
  [Term.Lam (xs, expand env' body)]
