open Core

module Map = Map.Make (Addr)
module Gph = Graph.Imperative.Digraph.Concrete (Addr)
module Topo = Graph.Topological.Make (Gph)

module Tbl = Hashtbl.Make (Addr)

let build_import_graph (trees : Code.tree list) =
  let import_graph = Gph.create () in

  let rec analyse_tree roots (tree : Code.tree) =
    let roots = Option.fold ~none:roots ~some:(fun x -> x :: roots) tree.addr in
    begin
      tree.addr |> Option.iter @@ fun addr ->
      Gph.add_vertex import_graph @@ User_addr addr
    end;
    tree.code |> List.iter @@ fun node ->
    match Asai.Range.(node.value) with
    | Code.Import (_, dep) ->
      roots |> List.iter @@ fun addr -> Gph.add_edge import_graph (User_addr dep) (User_addr addr)
    | Code.Subtree (addr, code) ->
      analyse_tree roots @@ Code.{tree with addr; code}
    | _ -> ()
  in

  trees |> List.iter (analyse_tree []);
  import_graph

type analysis =
  {transclusion_graph : Gph.t;
   link_graph : Gph.t;
   contributors : addr Tbl.t;
   author_pages : addr Tbl.t;
   bibliography : addr Tbl.t}


let new_analysis () =
  let size = 100 in
  {transclusion_graph = Gph.create ();
   link_graph = Gph.create ();
   author_pages = Tbl.create size;
   contributors = Tbl.create size;
   bibliography = Tbl.create size}

let rec analyze_nodes ~analysis scope : Sem.t -> unit =
  List.iter @@ fun located ->
  match Range.(located.value) with
  | Sem.Transclude (opts, addr) ->
    analyze_transclusion_opts ~analysis scope opts;
    Gph.add_edge analysis.transclusion_graph addr scope
  | Sem.Subtree (opts, subtree) ->
    analyze_transclusion_opts ~analysis scope opts;
    Gph.add_edge analysis.transclusion_graph subtree.fm.addr scope
  | Sem.Link (addr, title, _) ->
    Option.iter (analyze_nodes ~analysis scope) title;
    Gph.add_edge analysis.link_graph addr scope
  | Sem.Ref addr ->
    Gph.add_edge analysis.link_graph addr scope
  | Sem.Xml_tag (_, attrs, xs) ->
    begin
      attrs |> List.iter @@ fun (k, v) ->
      analyze_nodes ~analysis scope v
    end;
    analyze_nodes ~analysis scope xs
  | Sem.Math (_, x) ->
    analyze_nodes ~analysis scope x
  | Sem.Embed_tex {source; _} ->
    analyze_nodes ~analysis scope source
  | Sem.Query (opts, _) ->
    analyze_transclusion_opts ~analysis scope opts
  | Sem.If_tex (_, y) ->
    analyze_nodes ~analysis scope y
  | Sem.Prim (_, x) ->
    analyze_nodes ~analysis scope x
  | Sem.Object _ | Sem.Unresolved _ | Sem.Img _ | Sem.Text _ ->
    ()

and analyze_transclusion_opts ~analysis scope : Sem.transclusion_opts -> unit =
  function Sem.{title_override; _} ->
    title_override |> Option.iter @@ analyze_nodes ~analysis scope

let analyze_doc ~analysis scope (doc : Sem.tree) =
  analyze_nodes ~analysis scope doc.body;
  doc.fm.title |> Option.iter @@ analyze_nodes ~analysis scope;
  begin
    doc.fm.designated_parent |> Option.iter @@ fun parent ->
    if doc.fm.physical_parent = Some parent then () else
      Gph.add_edge analysis.link_graph parent scope
  end;
  begin
    doc.fm.authors |> List.iter @@ fun author ->
    Tbl.add analysis.author_pages author scope
  end;
  begin
    doc.fm.contributors |> List.iter @@ fun author ->
    Tbl.add analysis.author_pages author scope
  end;
  begin
    doc.fm.metas |> List.iter @@ fun (_, meta) ->
    analyze_nodes ~analysis scope meta
  end

let merge_bibliography ~analysis ~from_addr ~to_addr =
  Tbl.find_all analysis.bibliography from_addr |> List.iter @@ fun ref ->
  Tbl.add analysis.bibliography to_addr ref

let analyze_trees (trees : Sem.tree Map.t) : analysis =
  let analysis = new_analysis () in
  begin
    trees |> Map.iter @@ fun addr doc  ->
    Gph.add_vertex analysis.transclusion_graph addr;
    Gph.add_vertex analysis.link_graph addr;

    analyze_doc ~analysis addr doc;
    let task ref =
      match Map.find_opt ref trees with
      | Some (ref_doc : Sem.tree) when ref_doc.fm.taxon = Some "reference" ->
        Tbl.add analysis.bibliography addr ref
      | _ -> ()
    in
    Gph.iter_pred task analysis.link_graph addr;
  end;

  begin
    analysis.transclusion_graph |> Topo.iter @@ fun child_addr ->

    let handle_parent parent_addr =
      Map.find_opt child_addr trees |> Option.iter @@ fun (parent_doc : Sem.tree) ->
      match parent_doc.fm.taxon with
      | Some "reference" -> ()
      | _ ->
        begin
          parent_doc.fm.authors
          @ parent_doc.fm.contributors
          @ Tbl.find_all analysis.contributors child_addr
          |> List.iter @@ fun contributor ->
          Tbl.add analysis.contributors parent_addr contributor
        end;
        merge_bibliography ~analysis ~from_addr:child_addr ~to_addr:parent_addr
    in
    Gph.iter_succ handle_parent analysis.transclusion_graph child_addr
  end;

  analysis
