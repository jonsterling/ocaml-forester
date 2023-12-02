open Core

module Map = Map.Make (String)
module Gph = Graph.Imperative.Digraph.Concrete (String)
module Topo = Graph.Topological.Make (Gph)

module Tbl = Hashtbl.Make (String)

module type S =
sig
  val plant_tree : addr -> Code.t -> unit
  val analyze_trees : Sem.doc Map.t -> unit

  val transclusion_graph : Gph.t
  val link_graph : Gph.t
  val import_graph : Gph.t
  val contributors : addr Tbl.t
  val author_pages : addr Tbl.t
  val bibliography : addr Tbl.t
end

module Make () : S =
struct
  let size = 100
  let transclusion_graph = Gph.create ()
  let link_graph = Gph.create ()
  let import_graph = Gph.create ()
  let author_pages = Tbl.create 10
  let contributors = Tbl.create size
  let bibliography = Tbl.create size

  let rec analyze_nodes scope : Sem.t -> unit =
    List.iter @@ fun located ->
    match Range.(located.value) with
    | Sem.Transclude (opts, addr) ->
      analyze_transclusion_opts scope opts;
      Gph.add_edge transclusion_graph addr scope
    | Sem.Link {title; dest; _} ->
      Option.iter (analyze_nodes scope) title;
      Gph.add_edge link_graph dest scope
    | Sem.Xml_tag (_, attrs, xs) ->
      begin
        attrs |> List.iter @@ fun (k, v) ->
        analyze_nodes scope v
      end;
      analyze_nodes scope xs
    | Sem.Math (_, x) ->
      analyze_nodes scope x
    | Sem.Embed_tex {source; _} ->
      analyze_nodes scope source
    | Sem.Block (title, body) ->
      analyze_nodes scope title;
      analyze_nodes scope body
    | Sem.Query (opts, _) ->
      analyze_transclusion_opts scope opts
    | Sem.If_tex (_, y) ->
      analyze_nodes scope y
    | Sem.Prim (_, x) ->
      analyze_nodes scope x
    | Sem.Object _ | Sem.Unresolved _ | Sem.Img _ | Sem.Text _ ->
      ()

  and analyze_transclusion_opts scope : Sem.transclusion_opts -> unit =
    function Sem.{title_override; _} ->
      title_override |> Option.iter @@ analyze_nodes scope

  let analyze_doc scope (doc : Sem.doc) =
    analyze_nodes scope doc.body;
    doc.title |> Option.iter @@ analyze_nodes scope;
    begin
      doc.authors |> List.iter @@ fun author ->
      Tbl.add author_pages author scope
    end;
    begin
      doc.metas |> List.iter @@ fun (_, meta) ->
      analyze_nodes scope meta
    end

  let process_imports scope =
    List.iter @@ fun decl ->
    match Asai.Range.(decl.value) with
    | Code.Import (_, dep) ->
      Gph.add_edge import_graph dep scope
    | _ -> ()

  let plant_tree addr doc =
    Gph.add_vertex transclusion_graph addr;
    Gph.add_vertex link_graph addr;
    Gph.add_vertex import_graph addr;
    process_imports addr doc

  let analyze_trees (trees : Sem.doc Map.t) : unit =
    begin
      trees |> Map.iter @@ fun addr doc  ->
      analyze_doc addr doc;
      let task ref =
        match Map.find_opt ref trees with
        | Some (ref_doc : Sem.doc) when ref_doc.taxon = Some "reference" ->
          Tbl.add bibliography addr ref
        | _ -> ()
      in
      Gph.iter_pred task link_graph addr;
    end;
    transclusion_graph |> Topo.iter @@ fun addr ->
    let task addr' =
      Map.find_opt addr trees |> Option.iter @@ fun (doc : Sem.doc) ->
      match doc.taxon with
      | Some "reference" -> ()
      | _ ->
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

end
