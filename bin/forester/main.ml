open Forester
open Cmdliner

let version =
  Format.asprintf "%s" @@
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let run input_dirs root base_url dev =
  let module I =
  struct
    let size = 100
    let root = root
    let base_url = base_url
  end
  in

  let module F = Forest.Make (I) in
  let module P = Process.Make (F) in

  begin
    input_dirs |> List.iter @@
    P.process_dir ~dev
  end;

  F.render_trees ()

let arg_input_dirs =
  Arg.non_empty @@ Arg.pos_all Arg.file [] @@
  Arg.info [] ~docv:"INPUT_DIR"

let arg_root =
  let doc = "The address of the root tree, e.g. $(i,jms-0001); if this option is supplied, the tree $(i,jms-0001) will be rendered to $(i,output/index.xml)." in
  Arg.value @@ Arg.opt (Arg.some Arg.string) None @@
  Arg.info ["root"] ~docv:"ADDR" ~doc

let arg_dev =
  let doc = "Run forester in development mode; this will attach source file locations to the generated json." in
  Arg.value @@ Arg.flag @@ Arg.info ["dev"] ~doc

let arg_base_url =
  let doc = "Set the base URL for local hyperlinks." in
  Arg.value @@ Arg.opt (Arg.some Arg.string) None @@
  Arg.info ["base-url"] ~docv:"URL" ~doc


let cmd =
  let doc = "a tool for tending mathematical forests" in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command builds a hypertext $(b,forest) from trees stored in each $(i,INPUT_DIR) or any of its subdirectories; tree files are expected to be of the form $(i,addr.tree) where $(i,addr) is the global address of the tree. Note that the physical location of a tree is not taken into account, and two trees with the same address are not permitted.";
    `S Manpage.s_bugs;
    `P "Email bug reports to <~jonsterling/forester-discuss@lists.sr.ht>." ;
    `S Manpage.s_authors;
    `P "Jonathan Sterling"
  ]
  in
  let info = Cmd.info "forester" ~version ~doc ~man in
  Cmd.v info Term.(const run $ arg_input_dirs $ arg_root $ arg_base_url $ arg_dev)

let () =
  exit @@ Cmd.eval cmd
