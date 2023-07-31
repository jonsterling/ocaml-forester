open Forester

module Make (F : Forest.S) =
struct
  let rec process_file ~dev filename =
    if Filename.extension filename = ".tree" then
      let addr = Filename.chop_extension @@ Filename.basename filename in
      let sourcePath = if dev then Some (Unix.realpath filename) else None in
      F.plant_tree ~sourcePath addr @@ Parse.parse_file filename
    else if Sys.is_directory filename then 
      process_dir ~dev filename

  and process_dir ~dev dir =
    Sys.readdir dir |> Array.iter @@ fun filename ->
    process_file ~dev @@ dir ^ "/" ^ filename
end
