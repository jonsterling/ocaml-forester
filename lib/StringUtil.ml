let title_case_word word = 
  if String.length word > 3 then 
    String.capitalize_ascii word
  else
    word

let title_case str =
  let words = String.split_on_char ' ' str in
  String.concat " " @@ List.map title_case_word words
