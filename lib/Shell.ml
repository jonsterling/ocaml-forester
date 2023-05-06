let within_dir dir kont =
  let cwd = Sys.getcwd () in 
  Fun.protect ~finally:(fun _ -> Unix.chdir cwd) @@ fun _ -> 
  Unix.chdir dir;
  kont () 

let ensure_dir dir = 
  try Unix.mkdir dir 0o755 with 
  | Unix.Unix_error (Unix.EEXIST, _, _) -> 
    ()

let within_ensured_dir dir kont =
  ensure_dir dir; 
  within_dir dir kont

let ensure_remove_file fp = 
  try Unix.unlink fp with 
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    ()


module Proc = 
struct
  let popen = Unix.open_process_args_full
  let pclose = Unix.close_process_full
  let int_of_status s =
    match s with
    | Unix.WEXITED r -> r
    | Unix.WSIGNALED r -> r
    | Unix.WSTOPPED r -> r

  let dump ~inp ~out =
    try
      while true do
        output_char out (input_char inp)
      done
    with End_of_file -> 
      flush out

  exception Error of int

  let run cmd args =
    match Sys.command @@ String.concat " " @@ cmd :: args with 
    | 0 -> () 
    | code -> raise @@ Error code
end
