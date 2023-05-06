let within_dir dir kont =
  let cwd = Sys.getcwd () in 
  Fun.protect ~finally:(fun _ -> Unix.chdir cwd) @@ fun _ -> 
  Unix.chdir dir;
  kont () 

let ensure_dir dir = 
  try Unix.mkdir dir 0o755 with 
  | Unix.Unix_error (Unix.EEXIST, _, _) -> 
    ()

let ensure_remove_file fp = 
  try Unix.unlink fp with 
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    ()


module Proc = 
struct
  let popen = Unix.open_process_args_full
  let pclose = Unix.close_process_full

  let dump ~inp ~out =
    try
      while true do
        output_char out (input_char inp)
      done
    with End_of_file -> 
      flush out

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
    let ic, oc, ec as proc = 
      let cmd' = String.concat " " @@ cmd :: args in
      Unix.open_process_full cmd' (Unix.environment ())
    in

    let out_buf = Buffer.create 32 in 
    let err_buf = Buffer.create 32 in

    ignore (read_to_EOF out_buf ic);
    ignore (read_to_EOF err_buf ec);

    let s = Unix.close_process_full proc in
    match status_code s with 
    | 0 -> () 
    | code -> 
      Format.eprintf "%s" (Buffer.contents err_buf);
      raise @@ Error code
end
