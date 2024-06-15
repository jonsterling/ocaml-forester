open Bwd

(* Based on cmap_utf_8 from https://erratique.ch/software/uucp/doc/Uucp/Case/index.html#caseexamples *)
let title_case_word s =
  let did_uppercase = ref false in
  let rec loop buf s i max =
    if i > max then Buffer.contents buf else
      let dec = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar dec in
      let should_ignore = Uucp.Case.is_case_ignorable u || not (Uucp.Case.is_cased u) in
      let () =
        match should_ignore || !did_uppercase with
        | true ->
          Buffer.add_utf_8_uchar buf u
        | false ->
          did_uppercase := true;
          match Uucp.Case.Map.to_upper u with
          | `Self -> Buffer.add_utf_8_uchar buf u
          | `Uchars us -> List.iter (Buffer.add_utf_8_uchar buf) us
      in
      loop buf s (i + Uchar.utf_decode_length dec) max
  in
  let buf = Buffer.create @@ String.length s * 2 in
  loop buf s 0 @@ String.length s - 1

let sentence_case str =
  let words = String.split_on_char ' ' str in
  String.concat " " @@ List.mapi (fun i word -> if i = 0 then title_case_word word else word) words

let trim_newlines str =
  let rec process_lines lines =
    match lines with
    | [] -> []
    | "" :: lines -> process_lines lines
    | _ -> lines
  in

  let lines = String.split_on_char '\n' str in
  String.concat "\n" @@ List.rev @@ process_lines @@ List.rev @@ process_lines lines

let trim_trailing_whitespace str =
  let rec process_chars rstr =
    match rstr with
    | '\n' :: rstr -> process_chars rstr
    | ' ' :: rstr -> process_chars rstr
    | '\t' :: rstr -> process_chars rstr
    | _ -> List.rev rstr
  in
  let n = String.length str in
  let chars = List.rev @@ List.init n (String.get str) in
  String.of_seq @@ List.to_seq @@ process_chars @@ chars

let explode str =
  List.init (String.length str) (String.get str)

let implode chars =
  String.init (List.length chars) (List.nth chars)

let implode_bwd chars =
  implode (Bwd.to_list chars)
