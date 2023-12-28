open Forester
open Core

module S = Algaeff.Sequencer.Make (struct type t = Code.tree end)

let rec process_file ~dev fp =
  if Eio.Path.is_directory fp then
    process_dir ~dev fp
  else
    Eio.Path.split fp |> Option.iter @@ fun (dir, basename) ->
    if Filename.extension basename = ".tree" then
      let addr = Filename.chop_extension basename in
      let source_path = if dev then Eio.Path.native fp else None in
      let code = Parse.parse_file fp in
      S.yield {source_path; addr; code}

and process_dir ~dev fp =
  Eio.Path.with_open_dir fp @@ fun dir ->
  Eio.Path.read_dir dir |> List.iter @@ fun fp ->
  process_file ~dev Eio.Path.(dir / fp)

let read_trees_in_dirs ~dev dirs =
  S.run @@ fun () ->
  dirs |> List.iter (process_dir ~dev)
