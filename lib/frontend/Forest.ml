open Eio.Std
open Prelude
open Core
open Render

module T = Domainslib.Task

module Addr = String

module M = Analysis.Map
module Tbl = Analysis.Tbl
module Gph = Analysis.Gph

module type S =
sig
  val plant_tree : source_path:string option -> addr -> Code.doc -> unit
  val create_tree : dir:string -> dest:string -> prefix:string -> template:string option -> addr
  val complete : string -> (addr * string) Seq.t
  val render_trees : unit -> unit
end


module type I =
sig
  val env : Eio_unix.Stdenv.base
  val root : addr option
  val base_url : string option
  val ignore_tex_cache : bool
  val max_fibers : int
end


module Make (I : I) : S =
struct
  module LaTeX_queue = LaTeX_queue.Make (I)
  module A = Analysis.Make ()

  let size = 100

  let frozen = ref false
  let source_paths : (string, string) Hashtbl.t = Hashtbl.create size

  let run_renderer (docs : Sem.doc M.t) (analysis : Analysis.analysis) (body : unit -> 'a) : 'a =
    let module S = Set.Make (String) in
    let module H : Render_effect.Handler =
    struct
      let is_root addr =
        I.root = Some addr

      let route target addr =
        let ext =
          match target with
          | Render_effect.Xml -> "xml"
          | Render_effect.Rss -> "rss.xml"
        in
        let base =
          match is_root addr with
          | true -> "index"
          | false -> addr
        in
        Format.asprintf "%s.%s" base ext

      let source_path addr =
        Hashtbl.find_opt source_paths addr

      let get_doc addr =
        M.find_opt addr docs

      let enqueue_latex ~name ~packages ~source =
        LaTeX_queue.enqueue ~name ~packages ~source

      let addr_peek_title scope =
        match M.find_opt scope docs with
        | Some doc -> Sem.Doc.peek_title doc
        | None -> None

      let get_sorted_trees addrs : Sem.doc list =
        let find addr =
          match M.find_opt addr docs with
          | None -> []
          | Some doc -> [doc]
        in
        Sem.Doc.sort @@ List.concat_map find @@ S.elements addrs

      let get_all_links scope =
        get_sorted_trees @@ S.of_list @@ Gph.pred analysis.link_graph scope

      let backlinks scope =
        get_sorted_trees @@ S.of_list @@ Gph.succ analysis.link_graph scope

      let related scope =
        get_all_links scope |> List.filter @@ fun (doc : Sem.doc) ->
        doc.taxon <> Some "reference"

      let bibliography scope =
        get_sorted_trees @@
        S.of_list @@ Analysis.Tbl.find_all analysis.bibliography scope

      let parents scope =
        get_sorted_trees @@ S.of_list @@ Gph.succ analysis.transclusion_graph scope

      let children scope =
        get_sorted_trees @@ S.of_list @@ Gph.pred analysis.transclusion_graph scope

      let contributions scope =
        get_sorted_trees @@ S.of_list @@ Tbl.find_all analysis.author_pages scope

      let contributors scope =
        let doc = M.find scope docs in
        let authors = S.of_list doc.authors in
        let contributors = S.of_list @@ Tbl.find_all analysis.contributors scope in
        let proper_contributors =
          contributors |> S.filter @@ fun contr ->
          not @@ S.mem contr authors
        in
        let by_title = Compare.under addr_peek_title @@ Compare.option String.compare in
        let compare = Compare.cascade by_title String.compare in
        List.sort compare @@ S.elements proper_contributors

      let rec test_query query (doc : Sem.doc) =
        match query with
        | Query.Author [Range.{value = Sem.Text addr; _}] ->
          List.mem addr doc.authors
        | Query.Tag [{value = Sem.Text addr; _}] ->
          List.mem addr doc.tags
        | Query.Meta (key, value) ->
          List.mem (key, value) doc.metas
        | Query.Taxon [{value = Sem.Text taxon; _}] ->
          doc.taxon = Some taxon
        | Query.Or qs ->
          qs |> List.exists @@ fun q -> test_query q doc
        | Query.And qs ->
          qs |> List.for_all @@ fun q -> test_query q doc
        | Query.Not q ->
          not @@ test_query q doc
        | Query.True ->
          true
        | _ -> false

      let run_query query =
        get_sorted_trees @@ S.of_seq @@ Seq.map fst @@ M.to_seq @@
        M.filter (fun _ doc -> test_query query doc) docs
    end
    in
    let module Run = Render_effect.Run (H) in
    Run.run body

  let unexpanded_trees : (addr, Code.doc) Hashtbl.t = Hashtbl.create size

  let plant_tree ~(source_path : string option) addr (doc : Code.doc) : unit =
    assert (not !frozen);
    if Hashtbl.mem unexpanded_trees addr then
      Reporter.fatalf Duplicate_tree "duplicate tree at address `%s`" addr;
    source_path |> Option.iter @@ Hashtbl.add source_paths addr;
    A.plant_tree addr doc;
    Hashtbl.add unexpanded_trees addr doc

  let prepare_forest () : Sem.doc M.t * Analysis.analysis =
    frozen := true;

    let docs =
      begin
        let task addr (units, trees) =
          let doc = Hashtbl.find unexpanded_trees addr in
          let units, doc = Expand.expand_doc units addr doc in
          let doc = Eval.eval_doc doc in
          units, M.add addr doc trees
        in
        snd @@ Analysis.Topo.fold task A.import_graph (Expand.UnitMap.empty, M.empty)
      end
    in

    docs, A.analyze_trees docs

  let next_addr ~prefix docs =
    let keys =
      M.to_seq docs |> Seq.map fst |> Seq.filter_map @@ fun addr ->
      match String.split_on_char '-' addr with
      | [prefix'; str] when prefix' = prefix ->
        BaseN.Base36.int_of_string str
      | _ -> None
    in
    let next = 1 + Seq.fold_left max 0 keys in
    prefix ^ "-" ^ BaseN.Base36.string_of_int next

  let create_tree ~dir ~dest ~prefix ~template =
    let docs, _ = prepare_forest () in
    let next = next_addr docs ~prefix in
    let fname = next ^ ".tree" in
    let now = Date.now () in
    let template_content =
      match template with
      | None -> ""
      | Some name -> Eio.Path.load Eio.Path.(Eio.Stdenv.cwd I.env / "templates" / (name ^ ".tree"))
    in
    let body = Format.asprintf "\\date{%a}\n" Date.pp now in
    let create = `Exclusive 0o644 in
    let path = Eio.Path.(Eio.Stdenv.cwd I.env / dest / fname) in
    Eio.Path.save ~create path @@ body ^ template_content;
    next

  let complete prefix =
    prepare_forest ()
    |> fst
    |> M.filter_map (fun _ -> Sem.Doc.peek_title)
    |> M.filter (fun _ -> String.starts_with ~prefix)
    |> M.to_seq

  module E = Render_effect.Perform

  let render_doc ~cwd ~docs ~bib_fmt doc =
    Render_bibtex.render_bibtex ~base_url:I.base_url doc bib_fmt;
    Format.fprintf bib_fmt "\n";

    doc.addr |> Option.iter @@ fun addr ->
    let create = `Or_truncate 0o644 in
    let base_url = I.base_url in
    begin
      (* TODO: the XML output via Eio is overflowing!!! *)
      let ch = open_out @@ "output/" ^ E.route Xml addr in
      (* let path = Eio.Path.(cwd / "output" / E.route Xml addr) in *)
      (* Eio.Path.with_open_out ~create path @@ fun flow -> *)
      (* Eio.Buf_write.with_flow flow @@ fun w -> *)
      Fun.protect ~finally:(fun _ -> close_out ch) @@ fun _ ->
      let out = Xmlm.make_output @@ `Channel ch in
      (* Eio_util.xmlm_dest_of_writer w in *)
      Render_xml.render_doc_page ~base_url ~trail:(Some Emp) doc out
    end;
    begin
      base_url |> Option.iter @@ fun base_url ->
      let path = Eio.Path.(cwd / "output" / E.route Rss addr) in
      Eio.Path.with_open_out ~create path @@ fun flow ->
      Eio.Buf_write.with_flow flow @@ fun w ->
      let out = Xmlm.make_output @@ Eio_util.xmlm_dest_of_writer w in
      Render_rss.render_doc_page ~base_url doc out
    end;
    begin
      let path = Eio.Path.(cwd / "latex" / (addr ^ ".tex")) in
      Eio.Path.with_open_out ~create path @@ fun flow ->
      Eio.Buf_write.with_flow flow @@ fun w ->
      Render_latex.render_doc_page ~base_url doc @@ Eio_util.formatter_of_writer w
    end

  let render_json ~cwd docs =
    let create = `Or_truncate 0o644 in
    let json_path = Eio.Path.(cwd / "output" / "forest.json") in
    Eio.Path.with_open_out ~create json_path @@ fun json_sink ->
    Eio.Buf_write.with_flow json_sink @@ fun w ->
    let fmt = Eio_util.formatter_of_writer w in
    let docs = Sem.Doc.sort @@ List.of_seq @@ Seq.map snd @@ M.to_seq docs in
    Render_json.render_docs docs fmt

  let copy_theme ~env =
    let cwd = Eio.Stdenv.cwd env in
    Eio.Path.with_open_dir Eio.Path.(cwd / "theme") @@ fun theme ->
    Eio.Path.read_dir theme |> List.iter @@ fun fname ->
    let source = "theme/" ^ fname in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"output"

  let copy_assets ~env =
    let cwd = Eio.Stdenv.cwd env in
    Eio.Path.with_open_dir Eio.Path.(cwd / "assets") @@ fun assets ->
    Eio.Path.read_dir assets |> List.iter @@ fun fname ->
    let source = "assets/" ^ fname in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"build";
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"output";
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"latex"

  let copy_resources ~env =
    let cwd = Eio.Stdenv.cwd env in
    Eio.Path.with_open_dir Eio.Path.(cwd / "build") @@ fun build ->
    Eio.Path.read_dir build |> List.iter @@ fun fname ->
    let ext = Filename.extension fname in
    let fp = Format.sprintf "build/%s" fname in
    let dest_opt =
      match ext with
      | ".svg" -> Some "output/resources";
      | ".pdf" -> Some "latex/resources"
      | _ -> None
    in
    dest_opt |> Option.iter @@ fun dest_dir ->
    if not @@ Eio_util.file_exists Eio.Path.(cwd / dest_dir / fname) then
      Eio_util.copy_to_dir ~cwd ~env ~source:fp ~dest_dir

  let with_bib_fmt ~cwd kont =
    let create = `Or_truncate 0o644 in
    let bib_path = Eio.Path.(cwd / "latex" / "forest.bib") in
    Eio.Path.with_open_out ~append:true ~create bib_path @@ fun bib_sink ->
    Eio.Buf_write.with_flow bib_sink @@ fun bib_w ->
    kont @@ Eio_util.formatter_of_writer bib_w

  let render_trees () : unit =
    let docs, analysis = prepare_forest () in

    let env = I.env in
    let cwd = Eio.Stdenv.cwd env in

    Eio_util.ensure_dir @@ Eio.Path.(cwd / "build");
    Eio_util.ensure_dir_path cwd ["output"; "resources"];
    Eio_util.ensure_dir_path cwd ["latex"; "resources"];

    run_renderer docs analysis @@ fun () ->
    with_bib_fmt ~cwd @@ fun bib_fmt ->
    docs |> M.iter (fun _ -> render_doc ~cwd ~docs ~bib_fmt);
    render_json ~cwd docs;
    copy_assets ~env;
    copy_theme ~env;
    let _ = LaTeX_queue.process ~env in
    copy_resources ~env
end
