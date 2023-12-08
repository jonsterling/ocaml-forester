open Prelude
open Bwd
open Core

module E = Render_effect.Perform

module Printer = Xml_printer
type printer = Printer.printer

let render_rfc_822 date =
  let day = Option.value ~default:1 @@ Date.day date in
  let month =
    match Option.value ~default:1 @@ Date.month date with
    | 1 -> "Jan"
    | 2 -> "Feb"
    | 3 -> "Mar"
    | 4 -> "Apr"
    | 5 -> "May"
    | 6 -> "Jun"
    | 7 -> "Jul"
    | 8 -> "Aug"
    | 9 -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov"
    | 12 -> "Dec"
    | i -> failwith @@ Format.sprintf "render_rfc_822: invalid month %i" i
  in
  Format.asprintf "%i %s %i" day month @@ Date.year date

let render_tree_info ~base_url ~addr (doc : Sem.tree) : printer =
  Printer.seq [
    Printer.tag "title" [] [
      Printer.text @@ Option.value ~default:"Untitled" @@
      begin
        doc.title |> Option.map @@ fun title ->
        String_util.sentence_case @@
        Render_text.Printer.contents @@
        Render_text.render title
      end
    ];
    Printer.tag "link" [] [
      Printer.text @@ Format.asprintf "%s/%s" base_url @@ E.route Xml addr
    ];
    doc.dates |> Fun.flip List.nth_opt 0 |> Printer.option begin fun date ->
      Printer.tag "pubDate" [] [
        Printer.text @@ render_rfc_822 date
      ]
    end
  ]

let render_item ~base_url (doc : Sem.tree) : printer =
  match doc.addr with
  | None -> failwith "render_item: no addr"
  | Some addr ->
    Printer.tag "item" [] [
      render_tree_info ~base_url ~addr doc
    ]


let render_channel ~base_url (doc : Sem.tree) : printer =
  match doc.addr with
  | None -> failwith "render_channel: no addr"
  | Some addr ->
    let children = E.children addr in
    Printer.tag "channel" [] [
      render_tree_info ~base_url ~addr doc;
      Printer.iter (render_item ~base_url) children
    ]

let render_tree_page ~base_url (doc : Sem.tree) : printer =
  fun out ->
  Xmlm.output out @@ `Dtd None;
  Printer.tag "rss" ["version", "2.0"] [
    render_channel ~base_url doc
  ] out
