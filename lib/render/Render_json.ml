open Prelude
open Core

module E = Render_effect.Perform

module Printer =
struct
  module P0 =
  struct
    type out = Format.formatter
    let text txt fmt =
      Format.fprintf fmt "%s" txt
  end

  include Printer_kit.Kit (P0)

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

let render_tree (doc : Sem.tree) : Printer.t =
  match doc.fm.addr with
  | None -> Printer.nil
  | Some addr ->
    render_key addr @@ braces @@
    Printer.iter ~sep:comma (fun (k, x) -> render_key k x)
      ["title",
       begin
         match doc.fm.title with
         | None -> Printer.text "null"
         | Some title ->
           let title = Render_util.expand_title_with_parents doc title in
           let title_string =
             String.trim @@
             String_util.sentence_case @@
             Render_text.Printer.contents @@
             Render_text.render title
           in
           render_string_literal @@ Printer.text @@ escape title_string
       end;
       "taxon",
       begin
         match doc.fm.taxon with
         | None -> Printer.text "null"
         | Some taxon -> render_string_literal @@ Printer.text @@ String_util.sentence_case taxon
       end;
       "tags",
       begin
         squares @@
         Printer.iter ~sep:comma (fun tag -> render_string_literal @@ Printer.text tag) doc.fm.tags
       end;
       "route",
       render_string_literal @@ Printer.text @@
       E.route Xml addr]

let render_trees (docs : Sem.tree list) : Printer.t =
  braces @@ Printer.iter ~sep:comma render_tree docs
