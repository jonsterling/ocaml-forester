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
    let env = Unix.environment () in
    let ch_out, ch_in, ch_err as proc = popen cmd args env in 
    dump ~inp:ch_err ~out:stderr;
    match int_of_status @@ pclose proc with 
    | 0 -> ()
    | code -> raise @@ Error code
end

let render_svg ~name ~source =
  let tex_fp = Format.sprintf "%s.tex" name in
  let dvi_fp = Format.sprintf "%s.dvi" name in
  let svg_fp = Format.sprintf "%s.svg" name in

  within_ensured_dir "output" @@ fun _ ->
  within_ensured_dir "svgs" @@ fun _ -> 
  if not @@ Sys.file_exists svg_fp then 
    let tex_ch = open_out_gen [Open_creat; Open_trunc] 0o755 tex_fp in
    Fun.protect ~finally:(fun _ -> close_out tex_ch) @@ fun _ ->
    LaTeXTemplate.write tex_ch source;
    ensure_remove_file dvi_fp;
    Proc.run "latex" [|tex_fp|];
    ensure_remove_file tex_fp;
    Proc.run "dvisvgm" 
      [|"--exact"; 
        "--clipjoin"; 
        "--font-format=woff"; 
        "--bbox=papersize";
        "--zoom=1.5";
        dvi_fp;
        "--output=%f"|];
