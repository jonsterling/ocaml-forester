open Prelude

type printer = Xmlm.output -> unit

module P0 =
struct
  type out = Xmlm.output

  let text txt out =
    Xmlm.output out @@ `Data txt
end

include Printer_kit.Kit (P0)

let tag name attrs bdy : printer =
  let attrs' = attrs |> List.map @@ fun (k,v) -> ("", k), v in
  fun out ->
    Xmlm.output out @@ `El_start (("", name), attrs');
    seq ~sep:space bdy out;
    Xmlm.output out `El_end

let with_xsl stylesheet bdy : printer =
  fun out ->
  let line = Format.sprintf "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" stylesheet in
  Xmlm.output out @@ `Dtd (Some line);
  bdy out
