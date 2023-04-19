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
    val mutable frozen = false
    val expansionQueue : (addr * Syn.t) Queue.t = Queue.create ()
    val titles : Syn.t Tbl.t = Tbl.create 100
    val trees : Sem.doc Tbl.t = Tbl.create 100
    val vertical : Gph.t = Gph.create ()
    val horizontal : Gph.t = Gph.create ()
    val imports : Gph.t = Gph.create ()
    val macroTable : (addr, (Symbol.t, clo) Hashtbl.t) Hashtbl.t = Hashtbl.create 1000

    method private get_macros (addr : addr) : (Symbol.t, clo) Hashtbl.t =
      match Hashtbl.find_opt macroTable addr with
      | None ->
        let macros = Hashtbl.create 10 in
        Hashtbl.add macroTable addr macros;
        macros
      | Some macros -> macros

    method private global_resolver (addr : addr) : Expand.globals =
      Hashtbl.find_opt @@ self#get_macros addr

    method private process_metas scope : Syn.t -> unit =
      function
      | Syn.Seq xs -> List.iter (self#process_metas scope) xs
      | Syn.DefMacro (name, xs, code) ->
        let macros = self#get_macros scope in
        let clo = Clo (Env.empty, xs, code) in
        Hashtbl.add macros (User name) clo
      | Syn.Import dep ->
        Gph.add_edge imports dep scope
      | Syn.Title title ->
        Tbl.add titles scope title
      | _ -> ()

    method private expand_imports : unit =
      imports |> Topo.iter @@ fun addr ->
      let macros = self#get_macros addr in
      let task addr' =
        self#get_macros addr' |>
        Hashtbl.iter @@ Hashtbl.add macros
      in
      Gph.iter_pred task imports addr

    method private process_tree scope : Sem.t -> unit =
      function
      | Sem.Text _ -> ()
      | Sem.Transclude addr ->
        Gph.add_edge vertical addr scope
      | Sem.Wikilink (title, addr) ->
        self#process_tree scope title;
        Gph.add_edge horizontal addr scope
      | Sem.Tag (_, _, xs) ->
        xs |> List.iter @@ self#process_tree scope
      | Sem.Seq xs ->
        xs |> List.iter @@ self#process_tree scope
      | Sem.Math x ->
        self#process_tree scope x

    method private expand_tree addr tree = 
      let globals = self#global_resolver addr in
      let body = Expand.expand globals Env.empty tree in
      let title =
        match Tbl.find_opt titles addr with
        | None -> Sem.Text addr
        | Some title ->  Expand.expand globals Env.empty title
      in
      Tbl.add trees addr {title; body};

    method private expand_trees : unit =
      self#expand_imports;
      let rec loop () =
        match Queue.take expansionQueue with 
        | addr, tree -> self#expand_tree addr tree; loop () 
        | exception Queue.Empty -> ()
      in 
      loop ()

    method private process_trees : unit =
      self#expand_trees;
      trees |> Tbl.iter @@ fun addr Sem.{body; title} ->
      self#process_tree addr body;
      self#process_tree addr title

    method private render_env : Render.env =
      object(self)
        method route addr =
          addr ^ ".html"
        method transclude addr =
          let doc = Tbl.find trees addr in
          Render.render_doc self doc
      end

    method plant_tree addr (syn : Syn.t) : unit =
      assert (not frozen);
      let open Syn in
      Gph.add_vertex vertical addr;
      Gph.add_vertex imports addr;
      self#process_metas addr syn;
      Queue.push (addr, syn) expansionQueue

    method render_trees : unit =
      let open Sem in
      frozen <- true;
      let env = self#render_env in
      self#process_trees;
      trees |> Tbl.iter @@ fun addr doc ->
      let ch = open_out @@ "output/" ^ env#route addr in
      let out = Xmlm.make_output @@ `Channel ch in
      Render.render_doc_page env doc out

  end
