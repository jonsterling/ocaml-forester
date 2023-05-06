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
    val expansion_queue : (addr * Syn.t) Queue.t = Queue.create ()
    val titles : Syn.t Tbl.t = Tbl.create 100
    val taxa : string Tbl.t = Tbl.create 100
    val trees : Sem.doc Tbl.t = Tbl.create 100
    val vertical : Gph.t = Gph.create ()
    val horizontal : Gph.t = Gph.create ()
    val imports : Gph.t = Gph.create ()
    val macro_table : (addr, (Symbol.t, clo) Hashtbl.t) Hashtbl.t = Hashtbl.create 1000

    method private get_macros (addr : addr) : (Symbol.t, clo) Hashtbl.t =
      match Hashtbl.find_opt macro_table addr with
      | None ->
        let macros = Hashtbl.create 10 in
        Hashtbl.add macro_table addr macros;
        macros
      | Some macros -> macros

    method private global_resolver (addr : addr) : Expand.globals =
      Hashtbl.find_opt @@ self#get_macros addr

    method private process_metas_in_node scope : Syn.node -> unit =
      function
      | Syn.DefMacro (name, xs, code) ->
        let macros = self#get_macros scope in
        let clo = Clo (Env.empty, xs, code) in
        Hashtbl.add macros (User name) clo
      | Syn.Import dep ->
        Gph.add_edge imports dep scope
      | Syn.Title title ->
        Tbl.add titles scope title
      | Syn.Taxon taxon -> 
        Tbl.add taxa scope taxon
      | _ -> ()

    method private process_metas scope : Syn.t -> unit = 
      List.iter @@ self#process_metas_in_node scope

    method private expand_imports : unit =
      imports |> Topo.iter @@ fun addr ->
      let macros = self#get_macros addr in
      let task addr' =
        self#get_macros addr' |>
        Hashtbl.iter @@ Hashtbl.add macros
      in
      Gph.iter_pred task imports addr

    method private process_node scope : Sem.node -> unit =
      function
      | Sem.Text _ -> ()
      | Sem.Transclude addr ->
        Format.eprintf "processing transclusion of %s@." addr;
        Gph.add_edge vertical addr scope
      | Sem.Wikilink (title, addr) ->
        self#process_nodes scope title;
        Gph.add_edge horizontal addr scope
      | Sem.Tag (_, _, xs) ->
        xs |> List.iter @@ self#process_nodes scope
      | Sem.Math x ->
        self#process_nodes scope x
      | Sem.EmbedTeX x -> 
        self#process_nodes scope x
      | Sem.Group x ->
        self#process_nodes scope x

    method private process_nodes scope : Sem.t -> unit = 
      List.iter @@ self#process_node scope

    method private expand_tree addr tree = 
      let globals = self#global_resolver addr in
      let body = Expand.expand_nodes globals Env.empty tree in
      let title =
        match Tbl.find_opt titles addr with
        | None -> [Sem.Text addr]
        | Some title -> Expand.expand_nodes globals Env.empty title
      in
      Tbl.add trees addr {title; body};

    method private expand_trees : unit =
      self#expand_imports;
      let rec loop () =
        match Queue.take expansion_queue with 
        | addr, tree -> self#expand_tree addr tree; loop () 
        | exception Queue.Empty -> ()
      in 
      loop ()

    method private process_trees : unit =
      self#expand_trees;
      trees |> Tbl.iter @@ fun addr Sem.{body; title} ->
      self#process_nodes addr body;
      self#process_nodes addr title

    method private render_env : RenderHtml.env =
      object(self)
        method route addr =
          addr ^ ".html"

        method transclude addr =
          match Tbl.find trees addr with 
          | doc -> 
            RenderHtml.render_doc self doc
          | exception e ->
            Format.eprintf "Transclusion error: failed to find tree with address %s@." addr;
            raise e

        method enqueue_svg ~name ~source = 
          RenderSvg.render_svg ~name ~source
      end

    method plant_tree addr (syn : Syn.t) : unit =
      assert (not frozen);
      let open Syn in
      Gph.add_vertex vertical addr;
      Gph.add_vertex imports addr;
      self#process_metas addr syn;
      Queue.push (addr, syn) expansion_queue

    method render_trees : unit =
      let open Sem in
      frozen <- true;
      let env = self#render_env in
      self#process_trees;
      trees |> Tbl.iter @@ fun addr doc ->
      let ch = open_out @@ "output/" ^ env#route addr in
      let out = Xmlm.make_output @@ `Channel ch in
      RenderHtml.render_doc_page env doc out

  end
