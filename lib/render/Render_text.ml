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


let rec render_node : Sem.node Range.located -> Printer.t =
  fun node ->
  match node.value with
  | Sem.Text txt | Sem.Verbatim txt ->
    Printer.text txt
  | Sem.Math (_, xs) ->
    render xs
  | Sem.Xml_tag (name, _, body) ->
    render body
  | Sem.Link (addr, None, modifier) ->
    render @@
    Option.value ~default:[Range.locate_opt None @@ Sem.Text "Untitled"] @@
    Option.bind (E.get_doc addr) @@ fun doc ->
    Option.map (Sem.apply_modifier modifier) doc.fm.title
  | Sem.Link (addr, Some title, modifier) ->
    render @@ Sem.apply_modifier modifier title
  | Sem.If_tex (_, y) ->
    render y
  | Sem.Prim (_, x) ->
    render x
  | Sem.TeX_cs _ ->
    Printer.nil
  | _ ->
    Reporter.fatal ?loc:node.loc Unhandled_case "unhandled case in plain text renderer"

and render xs =
  Printer.iter render_node xs
