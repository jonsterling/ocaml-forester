open Types

module T = Domainslib.Task
module Y = Yuujinchou

module Addr = String
module Tbl = Hashtbl.Make (Addr)
module Gph = Graph.Imperative.Digraph.Concrete (Addr)
module Topo = Graph.Topological.Make (Gph)
module Clo = Graph.Traverse

class forest ~size ~root =
  object(self)
    val mutable frozen = false

    val unexpanded_trees : Code.doc Tbl.t = Tbl.create size
    val svg_queue : (string, string list * string) Hashtbl.t = Hashtbl.create 100

    val trees : Sem.doc Tbl.t = Tbl.create size
    val transclusion_graph : Gph.t = Gph.create ()
    val abspaths : string Tbl.t = Tbl.create size
    val link_graph : Gph.t = Gph.create ()
    val tag_graph : Gph.t = Gph.create ()
    val import_graph : Gph.t = Gph.create ()
    val author_pages : addr Tbl.t = Tbl.create 10
    val contributors : addr Tbl.t = Tbl.create size
    val bibliography : addr Tbl.t = Tbl.create size

    val export_table : Term.t Y.Trie.Untagged.t Tbl.t = Tbl.create size

    method private render_env =
      object(self)
        method is_root addr =
          root = Some addr

        method route addr =
          match self#is_root addr with 
          | true -> "index.xml"
          | false -> addr ^ ".xml"

        method get_absolute_path addr =
          Tbl.find_opt abspaths addr

        method get_doc  = 
          Tbl.find_opt trees

        method enqueue_svg ~name ~packages ~source = 
          if not @@ Hashtbl.mem svg_queue name then
            Hashtbl.add svg_queue name (packages, source)

        method get_sorted_trees addrs : Sem.doc list = 
          let module E =
          struct
            type t = string
            let peek_name c =
              match Tbl.find_opt trees c with
              | Some doc -> 
                begin 
                  match doc.title with 
                  | Sem.Text txt :: _ -> txt
                  | _ -> c
                end
              | None -> c

            let compare c0 c1 =
              String.compare (peek_name c0) (peek_name c1)
          end 
          in
          let module S = Set.Make (E) in
          S.elements (S.of_list addrs) |> List.concat_map @@ fun addr -> 
          match Tbl.find_opt trees addr with 
          | Some doc -> [doc]
          | None -> []

        method get_backlinks scope =
          self#get_sorted_trees @@ Gph.succ link_graph scope

        method get_all_links scope = 
          self#get_sorted_trees @@ Gph.pred link_graph scope

        method get_links scope = 
          self#get_all_links scope |> List.filter @@ fun (doc : Sem.doc) ->
          not (doc.taxon = Some "reference")

        method get_references scope = 
          self#get_sorted_trees @@ Tbl.find_all bibliography scope

        method get_parents scope =
          self#get_sorted_trees @@ Gph.succ transclusion_graph scope

        method get_pages_authored scope = 
          self#get_sorted_trees @@ Tbl.find_all author_pages scope

        method get_contributors scope = 
          let module C =
          struct
            type t = string
            let peek_name c =
              match Tbl.find_opt trees c with
              | Some doc -> 
                begin 
                  match doc.title with 
                  | Sem.Text txt :: _ -> txt
                  | _ -> c
                end
              | None -> c

            let compare c0 c1 =
              String.compare (peek_name c0) (peek_name c1)
          end 
          in
          let module S = Set.Make (C) in
          let doc = Tbl.find trees scope in
          let authors = S.of_list doc.authors in
          let contributors = S.of_list @@ Tbl.find_all contributors scope in
          let proper_contributors = 
            contributors |> S.filter @@ fun contr ->
            not @@ S.mem contr authors
          in
          S.elements proper_contributors

      end

    (* method private global_resolver (addr : addr) : Eval.globals =
       Hashtbl.find_opt @@ self#get_macros addr *)
    method private expand_transitive_contributors_and_bibliography : unit =
      begin
        trees |> Tbl.iter @@ fun addr _ -> 
        let task ref = 
          match Tbl.find_opt trees ref with 
          | None -> () 
          | Some doc -> 
            if doc.taxon = Some "reference" then 
              Tbl.add bibliography addr ref
        in 
        Gph.iter_pred task link_graph addr
      end;
      transclusion_graph |> Topo.iter @@ fun addr ->
      let task addr' = 
        let doc = Tbl.find trees addr in
        begin
          doc.authors @ Tbl.find_all contributors addr |> List.iter @@ fun contributor ->
          Tbl.add contributors addr' contributor
        end;
        begin
          let env = self#render_env in
          Tbl.find_all bibliography addr |> List.iter @@ fun ref ->
          Tbl.add bibliography addr' ref
        end
      in 
      Gph.iter_succ task transclusion_graph addr

    method private analyze_node scope : Sem.node -> unit =
      function
      | Sem.Text _ -> ()
      | Sem.Transclude (_, addr) ->
        Gph.add_edge transclusion_graph addr scope
      | Sem.Link {title; dest} ->
        self#analyze_nodes scope title;
        Gph.add_edge link_graph dest scope
      | Sem.Tag (_, _, xs) ->
        xs |> List.iter @@ self#analyze_nodes scope
      | Sem.Math (_, x) ->
        self#analyze_nodes scope x
      | Sem.EmbedTeX {source; _} -> 
        self#analyze_nodes scope source
      | Sem.Group (_, x) ->
        self#analyze_nodes scope x
      | Sem.Block (title, body) -> 
        self#analyze_nodes scope title;
        self#analyze_nodes scope body

    method private analyze_nodes scope : Sem.t -> unit = 
      List.iter @@ self#analyze_node scope

    method private expand_tree addr (doc : Code.doc) = 
      let fm, tree = doc in
      Resolver.Scope.run @@ fun () ->
      begin
        fm.decls |> List.iter @@ function
        | Code.Import dep -> 
          let import = Tbl.find export_table dep in
          Resolver.Scope.import_subtree ([], import)
        | Export dep -> 
          let import = Tbl.find export_table dep in
          Resolver.Scope.include_subtree ([], import)
        | Code.Def (path, ((xs,body) as macro)) ->
          let macro = Expander.expand_macro fm Env.empty macro in
          Resolver.Scope.include_singleton (path, (macro, ()));
          Resolver.Scope.export_visible (Y.Language.only path)
      end;

      let exports = Resolver.Scope.get_export () in
      Tbl.add export_table addr exports;

      let tree = Expander.expand fm Env.empty tree in

      let body =
        try 
          Eval.eval Env.empty tree 
        with
        | exn -> 
          Format.eprintf "Failed to eval %s: %a@." addr Term.pp tree;
          raise exn
      in

      let title =
        match fm.title with
        | None -> [Sem.Text addr]
        | Some title -> 
          Eval.eval Env.empty @@ 
          Expander.expand fm Env.empty title
      in 
      let metas = 
        fm.metas |> List.map @@ fun (key, body) ->
        key, Eval.eval Env.empty @@ Expander.expand fm Env.empty body
      in
      Sem.{title; body; addr; taxon = fm.taxon; authors = fm.authors; date = fm.date; metas}

    method private expand_trees : unit =
      import_graph |> Topo.iter @@ fun addr ->
      let edoc = Tbl.find unexpanded_trees addr in
      let doc = self#expand_tree addr edoc in
      Tbl.add trees addr doc

    method private analyze_trees : unit =
      self#expand_trees;
      begin
        trees |> Tbl.iter @@ fun addr Sem.{body; title; _} ->
        self#analyze_nodes addr body;
        self#analyze_nodes addr title
      end;
      self#expand_transitive_contributors_and_bibliography

    method plant_tree ~(abspath : string option) scope (doc : Code.doc) : unit =
      assert (not frozen);
      let frontmatter, body = doc in
      abspath |> Option.iter @@ Tbl.add abspaths scope;
      Gph.add_vertex transclusion_graph scope;
      Gph.add_vertex link_graph scope;
      Gph.add_vertex import_graph scope;
      Gph.add_vertex tag_graph scope;
      begin 
        begin 
          frontmatter.tags |> List.iter @@ fun addr -> 
          Gph.add_edge tag_graph addr scope
        end;
        begin 
          frontmatter.authors |> List.iter @@ fun author ->
          Tbl.add author_pages author scope
        end;
        begin 
          frontmatter.decls |> List.iter @@ function 
          | Code.Import dep | Code.Export dep -> 
            Gph.add_edge import_graph dep scope
          | _ -> ()
        end
      end;
      Tbl.add unexpanded_trees scope doc

    method private build_svgs : unit = 
      let n = Hashtbl.length svg_queue in
      let tasks = Array.make n `Uninitialized in

      begin
        let i = ref 0 in
        svg_queue |> Hashtbl.iter @@ fun name (packages, source) -> 
        tasks.(!i) <- `Task (name, packages, source);
        i := !i + 1
      end;

      Hashtbl.clear svg_queue;

      let worker i = 
        match tasks.(i) with 
        | `Task (name, packages, source) -> BuildSvg.build_svg ~name ~source ~packages
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
          RenderXml.render_doc_page (env :> RenderEnv.t) addr doc out
        end
      end;

      begin 
        let ch = open_out @@ "output/forest.json" in 
        Fun.protect ~finally:(fun _ -> close_out ch) @@ fun _ ->
        let fmt = Format.formatter_of_out_channel ch in
        let docs = List.of_seq @@ Tbl.to_seq_values trees in
        let env = (self#render_env :> RenderEnv.t) in
        RenderJson.render_docs env docs fmt
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
