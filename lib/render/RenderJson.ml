open Prelude
open Core

module E = RenderEff.Perform

module Printer =
struct
  module P0 =
  struct
    type out = Format.formatter
    let text txt fmt =
      Format.fprintf fmt "%s" txt
  end

  include PrinterKit.Kit (P0)

  let contents (printer : t) : string =
    Format.asprintf "%a" (fun fmt _ -> printer fmt) ()
end

let squares x =
  Printer.seq ~sep:Printer.space
    [Printer.text "["; x; Printer.text "]"]

let braces x =
  Printer.seq ~sep:Printer.space
    [Printer.text "{"; x; Printer.text "}"]

let comma = Printer.text ", "

let render_string_literal body =
  Printer.seq [Printer.text "\""; body; Printer.text "\""]


let render_key k p =
  Printer.seq ~sep:Printer.space
    [render_string_literal @@ Printer.text k;
     Printer.text ":";
     p]

let escape =
  Str.global_substitute (Str.regexp {|"|}) @@
  fun _ -> {|\"|}

let rec render nodes : Printer.t =
  Printer.iter render_node nodes

and render_node : Sem.node -> Printer.t =
  function
  | Sem.Text txt -> Printer.text @@ escape @@ StringUtil.sentence_case txt
  | Sem.Tag (_,body) -> render body
  | Sem.Link {title; _} -> render title
  | Sem.Transclude _ | Sem.EmbedTeX _ | Sem.Math _ | Sem.Block _ | Sem.Query _ -> Printer.nil


let render_doc (doc : Sem.doc) : Printer.t =
  match doc.addr with 
  | None -> Printer.nil 
  | Some addr -> 
    render_key addr @@ braces @@
    Printer.iter ~sep:comma (fun (k, x) -> render_key k x)
      ["title",
       begin
         match doc.title with
         | None -> Printer.text "null"
         | Some title -> render_string_literal @@ render title
       end;
       "taxon",
       begin
         match doc.taxon with
         | None -> Printer.text "null"
         | Some taxon -> render_string_literal @@ Printer.text @@ StringUtil.sentence_case taxon
       end;
       "tags", 
       begin 
         squares @@ 
         Printer.iter ~sep:comma (fun tag -> render_string_literal @@ Printer.text tag) doc.tags
       end;
       "route",
       render_string_literal @@ Printer.text @@
       E.route addr]

let render_docs (docs : Sem.doc list) : Printer.t =
  braces @@ Printer.iter ~sep:comma render_doc docs
