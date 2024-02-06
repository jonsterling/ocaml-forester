open Forester
open Core

let read_trees_in_dirs ~dev ?(ignore_malformed = false) dirs =
  ForestScanner.scan_directories dirs |> Seq.filter_map @@ fun fp ->
  Option.bind (Eio.Path.split fp) @@ fun (dir, basename) ->
  let addr = Filename.chop_extension basename in
  let source_path = if dev then Option.map Unix.realpath @@ Eio.Path.native fp else None in
  match Parse.parse_file fp with
  | code -> Some Code.{source_path; addr = Some addr; code}
  | exception exn ->
    if ignore_malformed then None else raise exn
