type t = {yyyy : int; mm : int; dd : int}

let to_seconds (date : t) : float = 
  fst @@ 
  Unix.mktime 
    {Unix.tm_sec=0;
     tm_min=0;
     tm_hour=0;
     tm_mday=date.dd;
     tm_mon=date.mm-1;
     tm_year=date.yyyy-1900;
     tm_wday=0;
     tm_yday=0;
     tm_isdst=false}

let compare (d0 : t) (d1 : t) = 
  compare (to_seconds d0) (to_seconds d1)

let parse str = 
  try 
    Scanf.sscanf str "%04d-%02d-%02d" @@ fun yyyy mm dd ->
    {yyyy; mm; dd}
  with _ -> 
    failwith @@ Format.sprintf "Invalid date string: %s" str

let pp fmt date =
  Format.fprintf fmt "%04d-%02d-%02d"
    date.yyyy date.mm date.dd

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
  Format.fprintf fmt "%a %i, %04d" 
    pp_month date.mm date.dd date.yyyy
