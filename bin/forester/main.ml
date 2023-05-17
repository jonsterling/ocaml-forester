open Forester
open Lexing

let colnum pos =
  pos.pos_cnum - pos.pos_bol - 1

let pp_pos fmt pos =
  Format.fprintf fmt "%s: line %i, column %i" pos.pos_fname pos.pos_lnum (colnum pos + 1)

let parse_channel filename ch =
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Parser.main Lexer.token lexbuf with
  | Parser.Error ->
    failwith @@ Format.asprintf "Parse error at %a" pp_pos lexbuf.lex_curr_p
  | Lexer.SyntaxError err ->
    failwith @@ Format.asprintf "Lexing error at %a : %s" pp_pos lexbuf.lex_curr_p err

let parse_file filename =
  let ch = open_in filename in
  Fun.protect ~finally:(fun _ -> close_in ch) @@ fun _ ->
  parse_channel filename ch

module Process (F : Forest.S) =
struct
  let process_file ~dev filename =
    if Filename.extension filename = ".tree" then
      let addr = Filename.chop_extension @@ Filename.basename filename in
      let abspath = if dev then Some (Unix.realpath filename) else None in
      F.plant_tree ~abspath addr @@ parse_file filename

  let process_dir ~dev dir =
    Sys.readdir dir |> Array.iter @@ fun filename ->
    process_file ~dev @@ dir ^ "/" ^ filename
end

let () =
  let input_dirs_ref = ref [] in
  let root_ref = ref "" in
  let dev_ref = ref false in
  let base_url_ref = ref "" in

  let usage_msg = "forester <dir> ..." in
  let args =
    ["--root", Arg.Set_string root_ref, "Set root tree";
     "--base-url", Arg.Set_string base_url_ref, "Set base URL";
     "--dev", Arg.Set dev_ref, "Development mode"]
  in
  let anon_fun dir = input_dirs_ref := dir :: !input_dirs_ref  in
  let () = Arg.parse args anon_fun usage_msg in

  let root =
    match !root_ref with
    | "" -> None
    | addr -> Some addr
  in

  let base_url = 
    match !base_url_ref with 
    | "" -> None 
    | url -> Some url
  in

  let module I =
  struct
    let size = 100
    let root = root
    let base_url = base_url
  end
  in

  let module F = Forest.Make (I) in
  let module P = Process (F) in

  begin
    !input_dirs_ref |> List.iter @@
    P.process_dir ~dev:!dev_ref
  end;

  F.render_trees ()
