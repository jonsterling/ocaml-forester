let under f compare x y =
  compare (f x) (f y)

let cascade compare0 compare1 x y =
  match compare0 x y with
  | 0 -> compare1 x y
  | i -> i

let option compare x y =
  match x, y with
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> 0
  | Some x, Some y -> compare x y

let sort_map f compare xs =
  List.sort compare @@ List.map f xs
