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
  let newline = text "\n"
end

module Metas = Map.Make (String)

let render_author author =
  match E.get_doc author with
  | None ->
    Printer.text author
  | Some doc ->
    match doc.title with
    | None ->
      Printer.text author
    | Some title ->
      Render_latex.render title

let render_authors : string list -> Printer.t =
  function
  | [] -> Printer.text "Anonymous"
  | authors ->
    Printer.iter ~sep:(Printer.text " and ") render_author authors

let render_title ~taxon title =
  match taxon with
  | Some taxon ->
    Format.dprintf "%s: %a"
      (String_util.sentence_case taxon)
      (Fun.flip Render_latex.render) title
  | None ->
    Render_latex.render @@ Sem.sentence_case title

let render_auto_bibtex ~base_url (doc : Sem.tree) : Printer.t =
  match doc.addr with
  | None -> Printer.nil
  | Some addr ->
    let contributors = E.contributors addr in
    Printer.seq ~sep:Printer.newline [
      Printer.nil;
      Format.dprintf "@misc{%s," addr;
      begin
        doc.title |> Printer.option @@ fun title ->
        Format.dprintf "title = {%a}," (Fun.flip @@ render_title ~taxon:doc.taxon) title
      end;
      Format.dprintf "author = {%a}," (Fun.flip render_authors) doc.authors;
      begin
        base_url |> Printer.option @@ fun url ->
        Format.dprintf "url = {%s/%s}," url @@ E.route Xml addr
      end;
      begin
        match contributors with
        | [] -> Printer.nil
        | _ ->
          let pp_sep fmt () = Format.fprintf fmt ", " in
          Format.dprintf "note = {With contributions from %a.},"
            (Format.pp_print_list ~pp_sep (Fun.flip render_author)) contributors
      end;
      Format.dprintf "}";
      Printer.nil
    ]

let render_bibtex ~base_url (doc : Sem.tree) : Printer.t =
  let metas = Metas.of_seq @@ List.to_seq doc.metas in
  match Metas.find_opt "bibtex" metas with
  | None ->
    render_auto_bibtex ~base_url doc
  | Some [{value = Text txt; _}] ->
    let lines = String.split_on_char '\n' txt in
    lines |> Printer.iter ~sep:Printer.newline @@ fun line ->
    Printer.text @@ String.trim line
  | Some other ->
    failwith @@ Format.asprintf "Unexpected contents of bibtex meta %a" Sem.pp other
