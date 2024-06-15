open Forester_core

let read_trees_in_dirs ~dev ?(ignore_malformed = false) dirs =
  ForestScanner.scan_directories dirs |> List.of_seq |> List.filter_map @@ fun fp ->
  Option.bind (Eio.Path.split fp) @@ fun (dir, basename) ->
  let addr = Filename.chop_extension basename in
  let source_path = if dev then Option.map Unix.realpath @@ Eio.Path.native fp else None in
  match Forester_frontend.Parse.parse_file fp with
  | Result.Ok code -> Some Code.{source_path; addr = Some addr; code}
  | Result.Error err -> None
  | exception exn ->
    if ignore_malformed then None else raise exn
