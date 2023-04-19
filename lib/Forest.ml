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
      let macros = self#get_macros addr in 
      let body = Expand.expand macros Map.empty doc in 
      let title = 
        match Tbl.find_opt titles addr with 
        | None -> Sem.Text addr
        | Some title ->  Expand.expand macros Map.empty title
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
      let env = self#render_env in
      self#process_trees; 
      trees |> Tbl.iter @@ fun addr doc -> 
      let ch = open_out @@ "output/" ^ env#route addr in 
      let out = Xmlm.make_output @@ `Channel ch in
      Render.render_doc_page env doc out;
  end
