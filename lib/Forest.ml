open Types

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

type macros = (string, closure) Hashtbl.t

class basic_forest =
  object(self)
    val trees : tree Tbl.t = Tbl.create 100
    val titles : tree Tbl.t = Tbl.create 100
    val vertical : Gph.t = Gph.create ()
    val horizontal : Gph.t = Gph.create ()
    val imports : Gph.t = Gph.create ()
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
  end
