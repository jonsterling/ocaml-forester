let title_case_word ix word = 
  if ix == 0 || String.length word > 3 then 
    String.capitalize_ascii word
  else
    word

let title_case str =
  let words = String.split_on_char ' ' str in
  String.concat " " @@ List.mapi title_case_word words
