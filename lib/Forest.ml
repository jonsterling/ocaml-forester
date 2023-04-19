open Types
open Bwd

module Addr =
struct
  type t = addr
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Tbl = Hashtbl.Make (Addr)
module Gph = Graph.Imperative.Digraph.Concrete (Addr)
module Topo = Graph.Topological.Make (Gph)
module Clo = Graph.Traverse
module Map = Map.Make (String)

type clo = Clo of Sem.t Map.t * string list * Syn.t

type macros = (string, clo) Hashtbl.t 

class forest =
  object(self)
    val expansionQueue : Syn.t Tbl.t = Tbl.create 100
    val titles : Syn.t Tbl.t = Tbl.create 100
    val trees : Sem.doc Tbl.t = Tbl.create 100
    val vertical : Gph.t = Gph.create ()
    val horizontal : Gph.t = Gph.create ()
    val imports : Gph.t = Gph.create ()
    val macroTable : (addr, macros) Hashtbl.t = Hashtbl.create 1000

    method private get_macros (addr : addr) : macros = 
      match Hashtbl.find_opt macroTable addr with 
      | None -> 
        let macros = Hashtbl.create 10 in 
        Hashtbl.add macroTable addr macros;
        macros
      | Some macros -> macros

    method private process_metas scope = 
      function 
      | Syn.Seq xs -> List.iter (self#process_metas scope) xs
      | Syn.DefMacro (name, xs, code) ->
        let macros = self#get_macros scope in
        let clo = Clo (Map.empty, xs, code) in
        Hashtbl.add macros name clo
      | Syn.Import dep -> 
        Gph.add_edge imports dep scope
      | Syn.Title title -> 
        Tbl.add titles scope title
      | _ -> ()

    method plant_tree addr (syn : Syn.t) = 
      let open Syn in 
      Gph.add_vertex vertical addr;
      Gph.add_vertex imports addr;
      self#process_metas addr syn;
      Tbl.add expansionQueue addr syn

    method private expand_imports =
      imports |> Topo.iter @@ fun addr -> 
      let macros = self#get_macros addr in 
      let task addr' = 
        self#get_macros addr' |> Hashtbl.iter @@ fun name clo -> 
        Hashtbl.add macros name clo
      in 
      Gph.iter_pred task imports addr

    method private expand_tree scope env = 
      function 
      | Syn.Text text ->
        Sem.Text text
      | Syn.Transclude addr -> 
        Sem.Transclude addr
      | Syn.Wikilink (title, dest) ->
        Sem.Wikilink (self#expand_tree scope env title, dest) 
      | Syn.Seq xs -> 
        Sem.Seq (List.map (self#expand_tree scope env) xs)
      | Syn.Tag (name, args) -> 
        let macros = self#get_macros scope in
        let args' = List.map (self#expand_tree scope env) args in
        begin
          match Map.find_opt name env, args with 
          | Some v, [] -> v
          | None, _ -> 
            begin
              match Hashtbl.find_opt macros name with 
              | Some (Clo (env', xs, body)) -> 
                let env'' = List.fold_right2 Map.add xs args' env' in
                self#expand_tree scope env'' body
              | None ->
                Sem.Tag (name, [], args')
            end
          | _ -> failwith "expand_tree"
        end
      | Syn.Math body -> 
        let body' = self#expand_tree scope env body in 
        Sem.Math body'
      | Syn.Title _ | Syn.DefMacro _ | Syn.Import _ -> Sem.Seq []

    method private process_tree scope = 
      function 
      | Sem.Text _ -> () 
      | Sem.Transclude addr -> 
        Gph.add_edge vertical addr scope
      | Sem.Wikilink (title, addr) -> 
        self#process_tree scope title;
        Gph.add_edge horizontal addr scope
      | Sem.Tag (_, attrs, xs) -> 
        xs |> List.iter (self#process_tree scope)
      | Sem.Seq xs ->
        xs |> List.iter (self#process_tree scope)
      | Sem.Math x -> 
        self#process_tree scope x

    method expand_trees = 
      self#expand_imports;
      expansionQueue |> Tbl.iter @@ fun addr doc ->
      let body = self#expand_tree addr Map.empty doc in 
      let title = 
        match Tbl.find_opt titles addr with 
        | None -> Sem.Seq []
        | Some title -> 
          self#expand_tree addr Map.empty title
      in
      Tbl.remove expansionQueue addr;
      Tbl.add trees addr {title; body}

    method process_trees = 
      self#expand_trees;
      trees |> Tbl.iter @@ fun addr Sem.{body; title} -> 
      self#process_tree addr body;
      self#process_tree addr title

    method render_env : Render.env =
      object(self)
        method route addr = 
          addr ^ ".html"
        method transclude addr = 
          let doc = Tbl.find trees addr in 
          Render.render_doc self doc
      end

    method render_trees : unit = 
      let open Sem in
      self#process_trees; 
      let out = Xmlm.make_output @@ `Channel stdout in
      trees |> Tbl.iter @@ fun _ doc -> 
      Render.render_doc_page self#render_env doc out;
      Format.print_newline ();
      Format.print_newline ();
  end
