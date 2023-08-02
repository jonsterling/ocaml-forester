type t = {yyyy : int; mm : int option; dd : int option}

(* approximate, only for sorting *)
let to_ptime (date : t) : Ptime.t =
  let dd = Option.value ~default:1 date.dd in
  let mm = Option.value ~default:1 date.mm in
  match Ptime.of_date (date.yyyy, mm, dd) with
  | None -> failwith "to_ptime"
  | Some t -> t

let compare (d0 : t) (d1 : t) =
  Ptime.compare (to_ptime d0) (to_ptime d1)

let parse_date str =
  match String.split_on_char '-' str with
  | yyyy :: rest ->
    let yyyy = int_of_string yyyy in
    begin
      match rest with
      | mm :: rest ->
        let mm = Some (int_of_string mm) in
        begin
          match rest with
          | [dd] ->
            let dd = Some (int_of_string dd) in
            {yyyy; mm; dd}
          | _ ->
            {yyyy; mm; dd = None}
        end
      | _ ->
        {yyyy; mm = None; dd = None}
    end
  | _ ->
    failwith @@ Format.sprintf "Invalid date string: %s" str

let parse str =
  match String.split_on_char 'T' str with
  | [date] -> parse_date date
  | date :: _ -> parse_date date
  | _ ->
    failwith @@ Format.sprintf "Invalid date string: %s" str

let pp fmt date =
  Format.fprintf fmt "%04d" date.yyyy;
  date.mm |> Option.iter @@ fun mm ->
  Format.fprintf fmt "-%02d" mm;
  date.dd |> Option.iter @@ fun dd ->
  Format.fprintf fmt "-%02d" dd

let pp_month fmt i =
  Format.fprintf fmt "%s" @@
  match i with
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ ->
    failwith @@ Format.sprintf "Invalid date: %i" i

let pp_human fmt date =
  match date.mm with
  | None ->
    Format.fprintf fmt "%04d" date.yyyy
  | Some mm ->
    match date.dd with
    | None ->
      Format.fprintf fmt "%a %04d" pp_month mm date.yyyy
    | Some dd ->
      Format.fprintf fmt "%a %i, %04d" pp_month mm dd date.yyyy
