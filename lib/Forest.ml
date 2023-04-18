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
    val expansionQueue : Syn.doc Tbl.t = Tbl.create 100
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

    method private process_meta addr = 
      function 
      | Syn.Import dep -> 
        Gph.add_edge imports dep addr
      | Syn.DefMacro (name, code) -> 
        let macros = self#get_macros addr in
        Hashtbl.add macros name code

    method private process_metas addr (metas : Syn.meta list) = 
      match metas with 
      | [] -> ()
      | meta :: metas ->
        self#process_meta addr meta; 
        self#process_metas addr metas

    method plant_tree addr (syn : Syn.doc) = 
      let open Syn in 
      Gph.add_vertex vertical addr;
      Gph.add_vertex imports addr;
      self#process_metas addr syn.metas;
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
      | Syn.Nil -> Sem.Nil 
      | Syn.Tag (name, attrs, body) ->
        let attrs' = attrs |> List.map (self#expand_attr scope env) in 
        let body' = self#expand_tree scope env body in 
        Sem.Tag (name, attrs', body')
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
      | Syn.Arg ix -> 
        Bwd.nth env ix

    method private expand_attr scope env (lbl, syn) = 
      lbl, self#expand_tree scope env syn

    method private process_tree scope = 
      function 
      | Sem.Text _ | Sem.Nil -> () 
      | Sem.Transclude addr -> 
        Gph.add_edge vertical addr scope
      | Sem.Wikilink (title, addr) -> 
        self#process_tree scope title;
        Gph.add_edge horizontal addr scope
      | Sem.Tag (_, attrs, body) -> 
        attrs |> List.iter (fun (_, x) -> self#process_tree scope x);
        self#process_tree scope body
      | Sem.Seq xs ->
        xs |> List.iter (self#process_tree scope)

    method expand_trees = 
      self#expand_imports;
      expansionQueue |> Tbl.iter @@ fun addr doc ->
      let body = self#expand_tree addr Bwd.Emp Syn.(doc.body) in 
      let title = self#expand_tree addr Bwd.Emp Syn.(doc.title) in
      Tbl.remove expansionQueue addr;
      Tbl.add trees addr {title; body}

    method process_trees = 
      self#expand_trees;
      trees |> Tbl.iter @@ fun addr doc -> 
      let open Sem in
      self#process_tree addr doc.body;
      self#process_tree addr doc.title
  end

(* type macros = (string, closure) Hashtbl.t *)
(* 
class basic_forest =
  object(self)
    val trees : tree Tbl.t = Tbl.create 100
    val titles : tree Tbl.t = Tbl.create 100
    val macroTable : (addr, macros) Hashtbl.t = 
      Hashtbl.create 1000
    val hasProcessed : (tree, unit) Hashtbl.t = 
      Hashtbl.create 1000
    val mutable frozen : bool = false

    method plant_tree addr tree =
      assert (not frozen);
      Gph.add_vertex vertical addr;
      Gph.add_vertex horizontal addr;
      Gph.add_vertex imports addr;
      Tbl.add trees addr tree

    method set_title addr tree = 
      assert (not frozen);
      Tbl.add titles addr tree

    method record_link ~src ~dest =
      assert (not frozen);
      Gph.add_edge horizontal dest src

    method record_translusion ~parent ~child =
      assert (not frozen);
      Gph.add_edge vertical child parent

    method private get_macros (addr : addr) : macros = 
      match Hashtbl.find_opt macroTable addr with 
      | None -> 
        let macros = Hashtbl.create 10 in 
        Hashtbl.add macroTable addr macros;
        macros
      | Some macros -> macros

    method def_macro addr ~(name : string) ~(body : closure) =
      assert (not frozen);
      let macros = self#get_macros addr in
      Hashtbl.add macros name body

    method import_macros ~at ~dep = 
      assert (not frozen);
      Gph.add_edge imports dep at

    method lookup_macro addr ~(name : string) ~(args : tree list) : tree =
      let macros = self#get_macros addr in 
      Hashtbl.find macros name args

    method lookup_tree addr =
      Tbl.find trees addr

    method lookup_backlinks addr = 
      if Gph.mem_vertex horizontal addr then 
        Gph.succ horizontal addr
      else 
        []

    method lookup_title addr = 
      Tbl.find_opt titles addr

    method process (addr : addr) (tree : tree) =
      match Hashtbl.find_opt hasProcessed tree with
      | None ->
        tree#process (self :> open_forest) addr;
        Hashtbl.add hasProcessed tree ()
      | Some () ->
        ()

    method freeze =
      Tbl.iter self#process trees;
      begin
        imports |> Topo.iter @@ fun addr -> 
        let macros = self#get_macros addr in 
        let task addr' = 
          self#get_macros addr' |> Hashtbl.iter @@ fun name clo -> 
          Hashtbl.add macros name clo
        in 
        Gph.iter_pred task imports addr
      end;
      frozen <- true

    method render_all fmt =
      if not frozen then 
        self#freeze;

      let task addr =
        Format.fprintf fmt "\n\n- Rendering %s\n" addr;
        let tree = new Tree.root addr in
        tree#render (self :> closed_forest) fmt
      in
      Topo.iter task vertical
  end *)
