open Core

module Forest_config =
struct
  type t =
    {trees : string list;
     assets : string list;
     theme : string;
     base_url : string option;
     root : string option;
     stylesheet : string}
  [@@deriving show]
end

let default_forest_config : Forest_config.t =
  {trees = ["trees"];
   assets = [];
   theme = "theme";
   base_url = None;
   root = None;
   stylesheet = "default.xsl"}

let parse_forest_config_file filename =
  let ch = open_in filename in
  Fun.protect ~finally:(fun _ -> close_in ch) @@ fun () ->
  let lexbuf = Lexing.from_channel ch in
  match Toml.Parser.parse lexbuf filename with
  | `Error (desc, { source; line; column; position }) ->
    Reporter.tracef "when parsing configuration file" @@ fun () ->
    let loc = Asai.Range.of_lexbuf ~source:(`File source) lexbuf in
    Reporter.fatalf ~loc Configuration_error "%s" desc
  | `Ok tbl ->
    let open Toml.Lenses in
    let forest = key "forest" |-- table in
    let trees =
      Option.value ~default:default_forest_config.trees @@
      get tbl (forest |-- key "trees" |-- array |-- strings)
    in
    let assets =
      Option.value ~default:default_forest_config.assets @@
      get tbl (forest |-- key "assets" |-- array |-- strings)
    in
    let theme =
      Option.value ~default:default_forest_config.theme @@
      get tbl (forest |-- key "theme" |-- string)
    in
    let stylesheet =
      Option.value ~default:default_forest_config.stylesheet @@
      get tbl (forest |-- key "stylesheet" |-- string)
    in
    let base_url = get tbl (forest |-- key "base_url" |-- string) in
    let root = get tbl (forest |-- key "root" |-- string) in
    Forest_config.{assets; trees; theme; base_url; root; stylesheet}

