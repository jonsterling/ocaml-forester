open Eio

let formatter_of_writer w =
  let out buf off len =
    Eio.Buf_write.string w buf ~off ~len
  in
  let flush () = () in
  Format.make_formatter out flush

let xmlm_dest_of_writer w =
  let write_byte b = Eio.Buf_write.char w @@ Char.chr b in
  `Fun write_byte

let null_sink () : Eio.Flow.sink =
  object
    inherit Eio.Flow.sink
    method copy _ = ()
  end

let ensure_dir path =
  try Eio.Path.mkdir ~perm:0o755 path with
  | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ()

let ensure_dir_path path dirs =
  let rec loop path =
    function
    | [] -> ()
    | dir :: dirs ->
      let path' = Eio.Path.(path / dir) in
      ensure_dir path';
      loop path' dirs
  in
  loop path dirs

let ensure_remove_file path =
  try Eio.Path.unlink path with
  | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let run_process ?(quiet = false) ~env ~cwd cmd =
  let mgr = Eio.Stdenv.process_mgr env in
  let outsink = Eio.Stdenv.stdout env in
  let outbuf = Buffer.create 100 in
  let errbuf = Buffer.create 100 in
  let errsink = Eio.Flow.buffer_sink errbuf in
  let outsink = Eio.Flow.buffer_sink outbuf in
  if not quiet then
    Eio.traceln "Running %s" (String.concat " " cmd);
  try Eio.Process.run ~cwd ~stdout:outsink ~stderr:errsink mgr cmd with
  | exn ->
    Eio.traceln "Error: %s" (Buffer.contents errbuf);
    Eio.traceln "Output: %s" (Buffer.contents outbuf);
    raise exn


let file_exists path =
  try Eio.Path.with_open_in path @@ fun _ -> true with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false

(* TODO: make this portable *)
let copy_to_dir ~env ~cwd ~source ~dest_dir =
  run_process ~quiet:true ~env ~cwd ["cp"; "-R"; source; dest_dir ^ "/" ]

