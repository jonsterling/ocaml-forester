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

type macros = (string, Syn.t) Hashtbl.t 
type env = Sem.t bwd

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
      | Syn.DefMacro (name, code) ->
        let macros = self#get_macros scope in
        Hashtbl.add macros name code
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
      | Syn.Text text -> Sem.Text text
      | Syn.Transclude addr -> Sem.Transclude addr
      | Syn.Wikilink (title, dest) -> Sem.Wikilink (self#expand_tree scope env title, dest) 
      | Syn.Tag (name, attrs, body) ->
        let body' = self#expand_tree scope env body in 
        Sem.Tag (name, attrs, body')
      | Syn.Seq xs -> Sem.Seq (List.map (self#expand_tree scope env) xs)
      | Syn.Macro (name, args) -> 
        let macros = self#get_macros scope in
        begin
          match Hashtbl.find_opt macros name with 
          | Some clo -> 
            let args' = List.map (self#expand_tree scope env) args in
            self#expand_tree scope (Bwd.append env args') clo
          | None -> 
            failwith @@ "Could not resolve macro named " ^ name ^ " in scope " ^ scope
        end
      | Syn.BVar ix -> 
        Bwd.nth env @@ ix - 1
      | Syn.Title _ | Syn.DefMacro _ | Syn.Import _ -> Sem.Seq []

    method private process_tree scope = 
      function 
      | Sem.Text _ -> () 
      | Sem.Transclude addr -> 
        Gph.add_edge vertical addr scope
      | Sem.Wikilink (title, addr) -> 
        self#process_tree scope title;
        Gph.add_edge horizontal addr scope
      | Sem.Tag (_, _, body) -> 
        self#process_tree scope body
      | Sem.Seq xs ->
        xs |> List.iter (self#process_tree scope)

    method expand_trees = 
      self#expand_imports;
      expansionQueue |> Tbl.iter @@ fun addr doc ->
      let body = self#expand_tree addr Bwd.Emp doc in 
      let title = 
        match Tbl.find_opt titles addr with 
        | None -> Sem.Seq []
        | Some title -> 
          self#expand_tree addr Bwd.Emp title
      in
      Tbl.remove expansionQueue addr;
      Tbl.add trees addr {title; body}

    method process_trees = 
      self#expand_trees;
      trees |> Tbl.iter @@ fun addr doc -> 
      let open Sem in
      self#process_tree addr doc.body;
      self#process_tree addr doc.title

    method render_trees : unit = 
      let open Sem in
      self#process_trees; 
      let env : Render.env = 
        object(self)
          method transclude addr = 
            let doc = Tbl.find trees addr in 
            Render.render self doc.body
        end
      in
      let out = Xmlm.make_output ~indent:(Some 2) (`Channel stdout) in
      trees |> Tbl.iter @@ fun _ doc -> 
      Xmlm.output out @@ `Dtd None;
      Xmlm.output out @@ `El_start (("", "body"), []);
      Render.render env doc.body out;
      Xmlm.output out `El_end;
      Format.print_newline ();
      Format.print_newline ();
  end
