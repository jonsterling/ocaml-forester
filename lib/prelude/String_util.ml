let index_of_first_ascii_char word =
  let rx = Str.regexp "\\([A-za-z]\\)" in
  try Option.some @@ Str.search_forward rx word 0 with _ -> None

let title_case_word ix word =
  if ix = 0 then
    begin
      match index_of_first_ascii_char word with
      | None -> word
      | Some i ->
        word |> String.mapi @@ fun j c ->
        if i = j then Char.uppercase_ascii c else c
    end
  else
    word

let sentence_case str =
  let words = String.split_on_char ' ' str in
  String.concat " " @@ List.mapi title_case_word words
