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

  let populate_import_graph scope =
    List.iter @@ fun decl ->
    match Asai.Range.(decl.value) with
    | Code.Import (_, dep) ->
      Gph.add_edge import_graph dep scope
    | _ -> ()

  let plant_tree addr doc =
    Gph.add_vertex transclusion_graph addr;
    Gph.add_vertex link_graph addr;
    Gph.add_vertex import_graph addr;
    populate_import_graph addr doc

  let merge_bibliography ~from_addr ~to_addr =
    Tbl.find_all bibliography from_addr |> List.iter @@ fun ref ->
    Tbl.add bibliography to_addr ref

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

    transclusion_graph |> Topo.iter @@ fun child_addr ->

    let handle_parent parent_addr =
      Map.find_opt child_addr trees |> Option.iter @@ fun (parent_doc : Sem.doc) ->
      match parent_doc.taxon with
      | Some "reference" -> ()
      | _ ->
        begin
          parent_doc.authors @ Tbl.find_all contributors child_addr |> List.iter @@ fun contributor ->
          Tbl.add contributors parent_addr contributor
        end;
        merge_bibliography ~from_addr:child_addr ~to_addr:parent_addr
    in
    Gph.iter_succ handle_parent transclusion_graph child_addr

end
