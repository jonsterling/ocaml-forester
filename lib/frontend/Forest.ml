open Prelude
open Core
open Render

module T = Domainslib.Task

module Addr = String
module Tbl = Hashtbl.Make (Addr)
module Gph = Graph.Imperative.Digraph.Concrete (Addr)
module Topo = Graph.Topological.Make (Gph)
module Clo = Graph.Traverse

module M = Map.Make (String)

module type S =
sig
  val plant_tree : abspath:string option -> addr -> Code.doc -> unit
  val render_trees : unit -> unit
end

module type I =
sig
  val size : int
  val root : addr option
  val base_url : string option
end

module Make (I : I) : S =
struct
  module LaTeXQueue = LaTeXQueue.Make (I)

  let frozen = ref false
  let unexpanded_trees : Code.doc Tbl.t = Tbl.create I.size

  let abspaths : string Tbl.t = Tbl.create I.size
  let import_graph : Gph.t = Gph.create ()

  let transclusion_graph : Gph.t = Gph.create ()
  let link_graph : Gph.t = Gph.create ()
  let tag_graph : Gph.t = Gph.create ()
  let author_pages : addr Tbl.t = Tbl.create 10
  let contributors : addr Tbl.t = Tbl.create I.size
  let bibliography : addr Tbl.t = Tbl.create I.size

  let run_renderer (docs : Sem.doc M.t) (body : unit -> 'a) : 'a =
    let module S = Set.Make (String) in
    let module H : RenderEff.Handler =
    struct
      let is_root addr =
        I.root = Some addr

      let route addr =
        match is_root addr with
        | true -> "index.xml"
        | false -> addr ^ ".xml"

      let abs_path addr =
        Tbl.find_opt abspaths addr

      let get_doc addr =
        M.find_opt addr docs

      let enqueue_latex ~name ~packages ~source =
        LaTeXQueue.enqueue ~name ~packages ~source

      let doc_peek_title (doc : Sem.doc) =
        match doc.title with
        | Some (Sem.Text txt :: _) -> Some txt
        | _ -> None

      let addr_peek_title scope =
        match M.find_opt scope docs with
        | Some doc -> doc_peek_title doc
        | None -> None

      let get_sorted_trees addrs : Sem.doc list =
        let by_date = Compare.under (fun x -> Sem.(x.date)) @@ Compare.option Date.compare in
        let by_title = Compare.under doc_peek_title @@ Compare.option String.compare in
        let by_addr = Compare.under (fun x -> Sem.(x.addr)) String.compare in
        let compare = Compare.cascade by_date @@ Compare.cascade by_title by_addr in
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

  let expand_transitive_contributors_and_bibliography (trees : Sem.doc M.t) : unit =
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

  let rec analyze_nodes scope : Sem.t -> unit =
    List.iter @@
    function
    | Sem.Text _ -> ()
    | Sem.Transclude (_, _, addr) ->
      Gph.add_edge transclusion_graph addr scope
    | Sem.Link {title; dest} ->
      analyze_nodes scope title;
      Gph.add_edge link_graph dest scope
    | Sem.Tag (_, xs) ->
      analyze_nodes scope xs
    | Sem.Math (_, x) ->
      analyze_nodes scope x
    | Sem.EmbedTeX {source; _} ->
      analyze_nodes scope source
    | Sem.Block (title, body) ->
      analyze_nodes scope title;
      analyze_nodes scope body

  let rec process_decl scope =
    function
    | Code.Tag tag -> 
      Gph.add_edge tag_graph tag scope
    | Code.Author author -> 
      Tbl.add author_pages author scope
    | Code.Import (_, dep) -> 
      Gph.add_edge import_graph dep scope
    | _ -> ()

  and process_decls scope =
    List.iter @@ process_decl scope


  let plant_tree ~(abspath : string option) scope (doc : Code.doc) : unit =
    assert (not !frozen);
    abspath |> Option.iter @@ Tbl.add abspaths scope;
    Gph.add_vertex transclusion_graph scope;
    Gph.add_vertex link_graph scope;
    Gph.add_vertex import_graph scope;
    Gph.add_vertex tag_graph scope;
    process_decls scope doc;
    Tbl.add unexpanded_trees scope doc


  let render_trees () : unit =
    let open Sem in
    frozen := true;

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
      analyze_nodes scope body;
      title |> Option.iter @@ analyze_nodes scope;
      metas |> List.iter @@ fun (_, meta) ->
      analyze_nodes scope meta
    end;

    expand_transitive_contributors_and_bibliography docs;

    Shell.ensure_dir "build";
    Shell.ensure_dir_path ["output"; "resources"];
    Shell.ensure_dir_path ["latex"; "resources"];

    run_renderer docs @@ fun () ->
    let module E = RenderEff.Perform in
    begin
      docs |> M.iter @@ fun _ doc ->
      begin
        let ch = open_out @@ "output/" ^ E.route doc.addr in
        Fun.protect ~finally:(fun _ -> close_out ch) @@ fun _ ->
        let out = Xmlm.make_output @@ `Channel ch in
        RenderXml.render_doc_page ~trail:Emp doc out
      end;
      begin 
        let ch = open_out @@ "latex/" ^ doc.addr ^ ".tex" in 
        Fun.protect ~finally:(fun _ -> close_out ch) @@ fun _ ->
        let fmt = Format.formatter_of_out_channel ch in
        RenderLaTeX.render_doc_page ~base_url:I.base_url doc fmt
      end;
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
          Shell.copy_file_to_dir ~source:fp ~dest_dir:"output";
          Shell.copy_file_to_dir ~source:fp ~dest_dir:"latex"
        end
    end;

    begin
      Shell.within_dir "build" @@ fun _ ->
      LaTeXQueue.process ()
    end;

    begin
      Sys.readdir "build" |> Array.iter @@ fun basename ->
      let ext = Filename.extension basename in
      let fp = Format.sprintf "build/%s" basename in
      match ext with 
      | ".svg" ->           
        Shell.copy_file_to_dir ~source:fp ~dest_dir:"output/resources/";
      | ".pdf" ->
        Shell.copy_file_to_dir ~source:fp ~dest_dir:"latex/resources/"
      | _ -> ()

    end;
end
