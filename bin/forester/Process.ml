open Forester

module Make (F : Forest.S) =
struct
  (* I would like to convert this code to use Eio's path primitives, but I don't see how to get around the need for 'realpath', etc. *)
  let rec process_file ~dev filename =
    if Filename.extension filename = ".tree" then
      let addr = Filename.chop_extension @@ Filename.basename filename in
      let sourcePath = if dev then Some (Eio_posix.Low_level.realpath filename) else None in
      F.plant_tree ~sourcePath addr @@ Parse.parse_file filename
    else if Sys.is_directory filename then
      process_dir ~dev filename

  and process_dir ~dev dir =
    Sys.readdir dir |> Array.iter @@ fun filename ->
    process_file ~dev @@ dir ^ "/" ^ filename
end
