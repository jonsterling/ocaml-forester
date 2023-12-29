module S = Algaeff.Sequencer.Make (struct type t = Eio.Fs.dir_ty Eio.Path.t end)

let rec process_file fp =
  if Eio.Path.is_directory fp then
    process_dir fp
  else
    Eio.Path.split fp |> Option.iter @@ fun (dir, basename) ->
    if Filename.extension basename = ".tree" && not @@ String.starts_with ~prefix:"." basename then
      S.yield fp

and process_dir fp =
  Eio.Path.with_open_dir fp @@ fun dir ->
  Eio.Path.read_dir dir |> List.iter @@ fun fp ->
  process_file Eio.Path.(dir / fp)

let scan_directories dirs =
  S.run @@ fun () ->
  dirs |> List.iter process_dir
