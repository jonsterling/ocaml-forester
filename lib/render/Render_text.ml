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

let rec render_node : Sem.node -> Printer.t =
  function
  | Sem.Text txt ->
    Printer.text txt
  | Sem.Math (_, xs) ->
    render xs
  | Sem.Tag (name, _, body) ->
    render body
  | Sem.Link {title = None; dest} ->
    render @@
    Option.value ~default:[Range.locate_opt None @@ Sem.Text dest] @@
    Option.bind (E.get_doc dest) @@ fun doc ->
    doc.title
  | Sem.Link {title = Some title; dest} ->
    render title
  | Sem.Transclude (_, addr) ->
    Printer.seq
      [Printer.text "\\transclude{";
       Printer.text addr;
       Printer.text "}"]
  | Sem.If_tex (_, y) ->
    render y
  | node ->
    Format.eprintf "missing case: %a@." Sem.pp_node node;
    failwith "Render_text.render_node"

and render_located_node located =
  fun fmt ->
  (* TODO: maybe no need for this *)
  Reporter.merge_loc Range.(located.loc) @@ fun () ->
  render_node located.value fmt

and render xs =
  Printer.iter render_located_node xs

and render_arg delim (arg : Sem.t) : Printer.t =
  match arg with
  | [] -> Printer.nil
  | _ ->
    let l, r =
      match delim with
      | Braces -> "{", "}"
      | Squares -> "[", "]"
      | Parens -> "(", ")"
    in
    Printer.seq
      [Printer.text l;
       render arg;
       Printer.text r]
