open Prelude

type printer = Xmlm.output -> unit

module P0 =
struct
  type out = Xmlm.output

  let text txt out =
    Xmlm.output out @@ `Data txt
end

include Printer_kit.Kit (P0)

let attr ?(ns = "") k v =
  (ns, k), v

let tag ?(ns = "") name attrs bdy : printer =
  fun out ->
  Xmlm.output out @@ `El_start ((ns, name), attrs);
  seq ~sep:space bdy out;
  Xmlm.output out `El_end

let with_xsl stylesheet bdy : printer =
  fun out ->
  let line = Format.sprintf "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" stylesheet in
  Xmlm.output out @@ `Dtd (Some line);
  bdy out

let document body : printer =
  fun out ->
  Xmlm.output out @@ `Dtd None;
  body out
