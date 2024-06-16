module S = Algaeff.Sequencer.Make (struct type t = Eio.Fs.dir_ty Eio.Path.t end)

let rec process_file fp =
  if Eio.Path.is_directory fp then
    process_dir fp
  else
    Eio.Path.split fp |> Option.iter @@ fun (dir, basename) ->
    if Filename.extension basename = ".tree" && not @@ String.starts_with ~prefix:"." basename then
      S.yield fp

and process_dir dir =
  try
    Eio.Path.read_dir dir |> List.iter @@ fun fp ->
    process_file Eio.Path.(dir / fp)
  with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let scan_directories dirs =
  S.run @@ fun () ->
  dirs |> List.iter @@ fun fp ->
  process_dir fp
