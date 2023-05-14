open Prelude
open Core
open Render

module T = Domainslib.Task
module Y = Yuujinchou

module Addr = String
module Tbl = Hashtbl.Make (Addr)
module Gph = Graph.Imperative.Digraph.Concrete (Addr)
module Topo = Graph.Topological.Make (Gph)
module Clo = Graph.Traverse

module M = Map.Make (String)
module S = Set.Make (String)

class svg_builder ~size =
  object
    val svg_queue : (string, string list * string) Hashtbl.t = Hashtbl.create size

    method enqueue ~name ~packages ~source =
      if not @@ Hashtbl.mem svg_queue name then
        Hashtbl.add svg_queue name (packages, source)

    method build : unit = 
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
  end


class forest ~size ~root =
  object(self)
    val mutable frozen = false

    val svg_builder = new svg_builder ~size:100
    val unexpanded_trees : Code.doc Tbl.t = Tbl.create size

    val abspaths : string Tbl.t = Tbl.create size
    val import_graph : Gph.t = Gph.create ()

    val transclusion_graph : Gph.t = Gph.create ()
    val link_graph : Gph.t = Gph.create ()
    val tag_graph : Gph.t = Gph.create ()
    val author_pages : addr Tbl.t = Tbl.create 10
    val contributors : addr Tbl.t = Tbl.create size
    val bibliography : addr Tbl.t = Tbl.create size

    method private run_renderer (docs : Sem.doc M.t) (body : unit -> 'a) : 'a = 
      let module H : RenderEff.Handler = 
      struct 
        let is_root addr =
          root = Some addr

        let route addr =
          match is_root addr with 
          | true -> "index.xml"
          | false -> addr ^ ".xml"

        let abs_path addr =
          Tbl.find_opt abspaths addr

        let get_doc addr = 
          M.find_opt addr docs

        let enqueue_svg ~name ~packages ~source = 
          svg_builder#enqueue ~name ~packages ~source

        let doc_peek_title (doc : Sem.doc) = 
          match doc.title with 
          | Some (Sem.Text txt :: _) -> Some txt
          | _ -> None

        let addr_peek_title scope = 
          match M.find_opt scope docs with
          | Some doc -> doc_peek_title doc
          | None -> None

        let get_sorted_trees addrs : Sem.doc list = 
          let by_taxon = Compare.under (fun x -> Sem.(x.taxon)) @@ fun x y ->
            match x, y with
            | Some "reference", Some "reference" -> 0
            | Some "reference", _ -> -1 
            | None, None -> 0
            | _ -> 1
          in 
          let by_date = Compare.under (fun x -> Sem.(x.date)) @@ Compare.option Date.compare in
          let by_title = Compare.under doc_peek_title @@ Compare.option String.compare in
          let by_addr = Compare.under (fun x -> Sem.(x.addr)) String.compare in
          let compare = Compare.cascade by_taxon @@ Compare.cascade by_date @@ Compare.cascade by_title by_addr in
          let find addr = 
            match M.find_opt addr docs with 
            | None -> [] 
            | Some doc -> [doc]
          in
          List.sort compare @@ List.concat_map find @@ S.elements addrs

        let get_all_links scope = 
          get_sorted_trees @@ S.of_list @@ Gph.pred link_graph scope

        let backlinks scope =
          get_sorted_trees @@ S.of_list @@ Gph.succ link_graph scope

        let related scope = 
          get_all_links scope |> List.filter @@ fun (doc : Sem.doc) ->
          not (doc.taxon = Some "reference")

        let bibliography scope = 
          get_sorted_trees @@ 
          S.of_list @@ Tbl.find_all bibliography scope

        let parents scope =
          get_sorted_trees @@ S.of_list @@ Gph.succ transclusion_graph scope

        let contributions scope = 
          get_sorted_trees @@ S.of_list @@ Tbl.find_all author_pages scope

        let contributors scope = 
          let doc = M.find scope docs in
          let authors = S.of_list doc.authors in
          let contributors = S.of_list @@ Tbl.find_all contributors scope in
          let proper_contributors = 
            contributors |> S.filter @@ fun contr ->
            not @@ S.mem contr authors
          in
          let by_title = Compare.under addr_peek_title @@ Compare.option String.compare in
          let compare = Compare.cascade by_title String.compare in
          List.sort compare @@ S.elements proper_contributors
      end
      in
      let module Run = RenderEff.Run (H) in
      Run.run body

    method private expand_transitive_contributors_and_bibliography (trees : Sem.doc M.t) : unit =
      begin
        trees |> M.iter @@ fun addr _ -> 
        let task ref = 
          match M.find_opt ref trees with 
          | None -> () 
          | Some (doc : Sem.doc) -> 
            if doc.taxon = Some "reference" then 
              Tbl.add bibliography addr ref
        in 
        Gph.iter_pred task link_graph addr
      end;
      transclusion_graph |> Topo.iter @@ fun addr ->
      let task addr' = 
        let doc = M.find addr trees in
        begin
          doc.authors @ Tbl.find_all contributors addr |> List.iter @@ fun contributor ->
          Tbl.add contributors addr' contributor
        end;
        begin
          Tbl.find_all bibliography addr |> List.iter @@ fun ref ->
          Tbl.add bibliography addr' ref
        end
      in 
      Gph.iter_succ task transclusion_graph addr

    method private analyze_nodes scope : Sem.t -> unit =
      List.iter @@ 
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
      | Sem.Block (title, body) -> 
        self#analyze_nodes scope title;
        self#analyze_nodes scope body

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
          | Code.Import (_, dep) -> 
            Gph.add_edge import_graph dep scope
          | _ -> ()
        end
      end;
      Tbl.add unexpanded_trees scope doc


    method render_trees : unit =
      let open Sem in
      frozen <- true;

      let docs = 
        begin 
          let task addr (units, trees) = 
            let doc = Tbl.find unexpanded_trees addr in
            let units, doc = Expand.expand_doc units addr doc in
            let doc = Eval.eval_doc doc in
            units, M.add addr doc trees
          in
          snd @@ Topo.fold task import_graph (Expand.UnitMap.empty, M.empty)
        end
      in

      begin
        docs |> M.iter @@ fun scope Sem.{body; title; metas; _} ->
        self#analyze_nodes scope body;
        title |> Option.iter @@ self#analyze_nodes scope;
        metas |> List.iter @@ fun (_, meta) -> 
        self#analyze_nodes scope meta
      end;

      self#expand_transitive_contributors_and_bibliography docs;

      Shell.ensure_dir "build";
      Shell.ensure_dir_path ["output"; "resources"];

      self#run_renderer docs @@ fun () -> 
      let module E = RenderEff.Perform in
      begin
        docs |> M.iter @@ fun addr doc ->
        begin
          let ch = open_out @@ "output/" ^ E.route addr in
          Fun.protect ~finally:(fun _ -> close_out ch) @@ fun _ ->
          let out = Xmlm.make_output @@ `Channel ch in
          RenderXml.render_doc_page addr doc out
        end
      end;

      begin 
        let ch = open_out @@ "output/forest.json" in 
        Fun.protect ~finally:(fun _ -> close_out ch) @@ fun _ ->
        let fmt = Format.formatter_of_out_channel ch in
        let docs = List.of_seq @@ Seq.map snd @@ M.to_seq docs in
        RenderJson.render_docs docs fmt
      end;

      begin 
        Sys.readdir "assets" |> Array.iter @@ fun basename ->
        let fp = Format.sprintf "assets/%s" basename in
        if Sys.is_directory fp then 
          failwith @@ 
          Format.sprintf "Expected flat directory structure in 'assets' but found '%s'" 
            basename
        else
          begin
            Shell.copy_file_to_dir ~source:fp ~dest_dir:"build";
            Shell.copy_file_to_dir ~source:fp ~dest_dir:"output"
          end
      end;

      begin
        Shell.within_dir "build" @@ fun _ ->
        svg_builder#build
      end;

      begin
        Sys.readdir "build" |> Array.iter @@ fun basename ->
        if Filename.extension basename = ".svg" then 
          let fp = Format.sprintf "build/%s" basename in
          Shell.copy_file_to_dir ~source:fp ~dest_dir:"output/resources/"
      end;

  end
