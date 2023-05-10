open Types

module T = Domainslib.Task

module Addr = String
module Tbl = Hashtbl.Make (Addr)
module Gph = Graph.Imperative.Digraph.Concrete (Addr)
module Topo = Graph.Topological.Make (Gph)
module Clo = Graph.Traverse

let expand_tree globals addr (doc : Expr.doc) = 
  let fm, tree = doc in
  let body = Expander.expand globals Env.empty tree in
  let title =
    match fm.title with
    | None -> [Sem.Text addr]
    | Some title -> Expander.expand globals Env.empty title
  in
  Sem.{title; body; addr; taxon = fm.taxon; authors = fm.authors; date = fm.date}

class forest ~size =
  object(self)
    val mutable frozen = false

    val expansion_queue : (addr * Expr.doc) Queue.t = Queue.create ()
    val svg_queue : (string, string) Hashtbl.t = Hashtbl.create 100

    val trees : Sem.doc Tbl.t = Tbl.create size
    val transclusion_graph : Gph.t = Gph.create ()
    val link_graph : Gph.t = Gph.create ()
    val tag_graph : Gph.t = Gph.create ()
    val import_graph : Gph.t = Gph.create ()
    val author_pages : addr Tbl.t = Tbl.create 10

    val macro_table : (addr, (Symbol.t, clo) Hashtbl.t) Hashtbl.t = 
      Hashtbl.create size

    method private render_env : RenderHtml.env =
      object(self)
        method route addr =
          addr ^ ".html"

        method get_doc  = 
          Tbl.find_opt trees

        method enqueue_svg ~name ~source = 
          if not @@ Hashtbl.mem svg_queue name then
            Hashtbl.add svg_queue name source

        (* TODO: sort by dates once we have them *)
        method private get_sorted_trees addrs : Sem.doc list = 
          let unsorted =           
            addrs |> List.concat_map @@ fun addr ->
            match Tbl.find_opt trees addr with 
            | Some doc -> [doc]
            | None -> []
          in
          List.sort Sem.Doc.compare_for_sorting unsorted

        method get_backlinks scope =
          self#get_sorted_trees @@ Gph.succ link_graph scope

        method get_links scope = 
          self#get_sorted_trees @@ Gph.pred link_graph scope

        method get_parents scope =
          self#get_sorted_trees @@ Gph.succ transclusion_graph scope

        method get_pages_authored scope = 
          self#get_sorted_trees @@ Tbl.find_all author_pages scope

      end

    method private get_macros (addr : addr) : (Symbol.t, clo) Hashtbl.t =
      match Hashtbl.find_opt macro_table addr with
      | None ->
        let macros = Hashtbl.create 10 in
        Hashtbl.add macro_table addr macros;
        macros
      | Some macros -> macros

    method private global_resolver (addr : addr) : Expander.globals =
      Hashtbl.find_opt @@ self#get_macros addr

    method private analyze_frontmatter scope (fm : Expr.frontmatter) = 
      let macros = self#get_macros scope in 
      begin 
        fm.tags |> List.iter @@ fun addr -> 
        Gph.add_edge tag_graph addr scope
      end;
      begin
        fm.imports |> List.iter @@ fun dep -> 
        Gph.add_edge import_graph dep scope
      end;
      begin 
        fm.macros |> List.iter @@ fun (name, (xs, body)) -> 
        let clo = Clo (Env.empty, xs, body) in 
        Hashtbl.add macros (User name) clo
      end;
      begin 
        fm.authors |> List.iter @@ fun author ->
        Tbl.add author_pages author scope
      end

    method private expand_imports : unit =
      import_graph |> Topo.iter @@ fun addr ->
      let macros = self#get_macros addr in
      let task addr' =
        self#get_macros addr' |>
        Hashtbl.iter @@ Hashtbl.add macros
      in
      Gph.iter_pred task import_graph addr

    method private analyze_node scope : Sem.node -> unit =
      function
      | Sem.Text _ -> ()
      | Sem.Transclude addr ->
        Gph.add_edge transclusion_graph addr scope
      | Sem.Link {title; addr} ->
        self#analyze_nodes scope title;
        Gph.add_edge link_graph addr scope
      | Sem.Tag (_, _, xs) ->
        xs |> List.iter @@ self#analyze_nodes scope
      | Sem.Math (_, x) ->
        self#analyze_nodes scope x
      | Sem.EmbedTeX x -> 
        self#analyze_nodes scope x
      | Sem.Group (_, x) ->
        self#analyze_nodes scope x

    method private analyze_nodes scope : Sem.t -> unit = 
      List.iter @@ self#analyze_node scope

    method private expand_trees : unit =
      self#expand_imports;
      let rec loop () =
        Queue.take_opt expansion_queue |> Option.iter @@ fun (addr, doc) ->
        let globals = self#global_resolver addr in
        let doc = expand_tree globals addr doc in
        Tbl.add trees addr doc;
        loop () 
      in 
      loop ()

    method private analyze_trees : unit =
      self#expand_trees;
      trees |> Tbl.iter @@ fun addr Sem.{body; title; _} ->
      self#analyze_nodes addr body;
      self#analyze_nodes addr title

    method plant_tree addr (doc : Expr.doc) : unit =
      assert (not frozen);
      let frontmatter, body = doc in
      Gph.add_vertex transclusion_graph addr;
      Gph.add_vertex link_graph addr;
      Gph.add_vertex import_graph addr;
      Gph.add_vertex tag_graph addr;
      self#analyze_frontmatter addr frontmatter;
      Queue.push (addr, doc) expansion_queue

    method private build_svgs : unit = 
      let n = Hashtbl.length svg_queue in
      let tasks = Array.make n `Uninitialized in

      begin
        let i = ref 0 in
        svg_queue |> Hashtbl.iter @@ fun name source -> 
        tasks.(!i) <- `Task (name, source);
        i := !i + 1
      end;

      Hashtbl.clear svg_queue;

      let worker i = 
        match tasks.(i) with 
        | `Task (name, source) -> BuildSvg.build_svg ~name ~source 
        | `Uninitialized -> failwith "Unexpected uninitialized task in SVG queue"
      in 

      let pool = T.setup_pool ~num_domains:10 () in
      begin
        T.run pool @@ fun _ ->
        T.parallel_for pool ~start:0 ~finish:(n-1) ~body:worker
      end;
      T.teardown_pool pool

    method render_trees : unit =
      let open Sem in
      frozen <- true;
      let env = self#render_env in
      self#analyze_trees;

      Shell.ensure_dir "build";
      Shell.ensure_dir_path ["output"; "resources"];

      begin
        trees |> Tbl.iter @@ fun addr doc ->
        begin
          let ch = open_out @@ "output/" ^ env#route addr in
          Fun.protect ~finally:(fun _ -> close_out ch) @@ fun _ ->
          let out = Xmlm.make_output @@ `Channel ch in
          RenderHtml.render_doc_page env addr doc out
        end
      end;

      begin
        Shell.within_dir "build" @@ fun _ ->
        self#build_svgs
      end;

      begin 
        Sys.readdir "assets" |> Array.iter @@ fun basename ->
        let fp = Format.sprintf "assets/%s" basename in
        if Sys.is_directory fp then 
          failwith @@ 
          Format.sprintf "Expected flat directory structure in 'assets' but found '%s'" 
            basename
        else
          Shell.copy_file_to_dir ~source:fp ~dest_dir:"output"
      end;

      begin
        Sys.readdir "build" |> Array.iter @@ fun basename ->
        if Filename.extension basename = ".svg" then 
          let fp = Format.sprintf "build/%s" basename in
          Shell.copy_file_to_dir ~source:fp ~dest_dir:"output/resources/"
      end;

  end
