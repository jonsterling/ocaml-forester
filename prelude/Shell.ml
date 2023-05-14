let within_dir dir kont =
  let cwd = Sys.getcwd () in 
  Fun.protect ~finally:(fun _ -> Unix.chdir cwd) @@ fun _ -> 
  Unix.chdir dir;
  kont () 

let ensure_dir dir =
  try Unix.mkdir dir 0o755 with 
  | Unix.Unix_error (Unix.EEXIST, _, _) -> 
    ()

let ensure_dir_path dirs = 
  let rec loop prefix =
    function 
    | [] -> () 
    | dir :: dirs -> 
      let dir' = Format.sprintf "%s/%s" prefix dir in
      ensure_dir dir';
      loop dir' dirs
  in 
  loop "." dirs

let ensure_remove_file fp = 
  try Unix.unlink fp with 
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    ()

module Chan = 
struct 
  let dump ~inp ~out =
    try
      while true do
        output_char out (input_char inp)
      done
    with End_of_file -> 
      flush out
end

module Proc = 
struct
  let popen = Unix.open_process_args_full
  let pclose = Unix.close_process_full

  exception Error of int

  let status_code s =
    match s with
    | Unix.WEXITED r -> r
    | Unix.WSIGNALED r -> r
    | Unix.WSTOPPED r -> r

  let append_to_buffer b c =
    Buffer.add_channel b c 1

  let read_to_EOF b c =
    try
      while true do
        append_to_buffer b c
      done
    with End_of_file -> ()


  let run cmd args =
    let cmd' = String.concat " " @@ cmd :: args in
    let ic, oc, ec as proc = 
      Format.eprintf "Running %s@." cmd';
      Unix.open_process_full cmd' (Unix.environment ())
    in

    let out_buf = Buffer.create 32 in 
    let err_buf = Buffer.create 32 in

    read_to_EOF out_buf ic;
    read_to_EOF err_buf ec;

    let s = Unix.close_process_full proc in
    match status_code s with 
    | 0 -> () 
    | code -> 
      failwith @@ 
      Format.sprintf "ERROR RUNNING [%s]: %s" cmd' (Buffer.contents err_buf);
end

let copy_file ~source ~dest =
  let src_ch = open_in source in 
  Fun.protect ~finally:(fun _ -> close_in src_ch) @@ fun _ ->
  let dst_ch = open_out dest in
  Fun.protect ~finally:(fun _ -> close_out dst_ch) @@ fun _ ->
  Chan.dump ~inp:src_ch ~out:dst_ch

let copy_file_to_dir ~source ~dest_dir =
  let dest = Format.sprintf "%s/%s" dest_dir @@ Filename.basename source in
  copy_file ~source ~dest
