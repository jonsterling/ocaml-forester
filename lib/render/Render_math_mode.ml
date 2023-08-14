open Prelude
open Core

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
  | Sem.Math(_, xs) ->
    render xs
  | Sem.Tag (name, _, body) ->
    render_tag name body
  | node ->
    Format.eprintf "missing case: %a@." Sem.pp_node node;
    failwith "Render_math_mode.render_node"

and render_tag name body =
  Printer.seq
    [Printer.text "\\";
     Printer.text name;
     render_arg Braces body]

and render xs =
  Printer.iter ~sep:Printer.space render_node xs

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
