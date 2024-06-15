open Forester_prelude
open Forester_core

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

type cfg = {tex : bool}

(* TeXBook Ch. 3 *)
type cs_type = Word | Symbol

let cs_symbol_rx = Str.regexp {|^[^A-Za-z]$|}

let cs_type name =
  if Str.string_match cs_symbol_rx name 0 then
    Symbol
  else
    Word

let cs_separator =
  function
  | Word -> Printer.space
  | Symbol -> Printer.nil

let rec render_node ~cfg : Sem.node Range.located -> Printer.t =
  fun located ->
  match located.value with
  | Sem.Text txt | Sem.Verbatim txt ->
    Printer.text txt
  | Sem.Math (mode, xs) ->
    render ~cfg xs;
  | Sem.Xml_tag (_, _, body) ->
    render ~cfg body
  | Sem.If_tex (x , y) ->
    if cfg.tex then render ~cfg x else render ~cfg y
  | Sem.TeX_cs (Symbol x) ->
    Printer.text @@ Format.sprintf "\\%c" x
  | Sem.TeX_cs (Word x) ->
    Printer.text @@ Format.sprintf "\\%s " x
  | node ->
    Reporter.fatalf ?loc:located.loc Type_error "Render_TeX_like: cannot render this kind of object"

and render ~cfg =
  Printer.iter (render_node ~cfg)
