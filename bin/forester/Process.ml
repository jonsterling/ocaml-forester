open Forester
open Core

type tree =
  {source_path : string option;
   addr : addr;
   code : Code.t}

module S = Algaeff.Sequencer.Make (struct type t = tree end)


(* I would like to convert this code to use Eio's path primitives, but I don't see how to get around the need for 'realpath', etc. *)

let rec process_file ~dev filename =
  if Filename.extension filename = ".tree" then
    let addr = Filename.chop_extension @@ Filename.basename filename in
    let source_path = if dev then Some (Eio_posix.Low_level.realpath filename) else None in
    let code = Parse.parse_file filename in
    S.yield {source_path; addr; code}
  else if Sys.is_directory filename then
    process_dir ~dev filename

and process_dir ~dev dir =
  Sys.readdir dir |> Array.iter @@ fun filename ->
  process_file ~dev @@ dir ^ "/" ^ filename


let read_trees_in_dirs ~dev dirs =
  S.run @@ fun () ->
  dirs |> List.iter (process_dir ~dev)

let read_trees_in_dir ~dev dir =
  read_trees_in_dirs ~dev [dir]
