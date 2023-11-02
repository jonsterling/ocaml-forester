include Asai.Range

let pp_located pp_arg fmt (x : 'a located) =
  pp_arg fmt x.value

let position_is_leq (pos1 : Asai.Range.position) (pos2 : Asai.Range.position) : bool =
  assert (pos1.source = pos2.source);
  pos1.offset <= pos2.offset

let min_position pos1 pos2 =
  if position_is_leq pos1 pos2 then pos1 else pos2

let max_position pos1 pos2 =
  if position_is_leq pos1 pos2 then pos2 else pos1

let merge_ranges (loc1 : Asai.Range.t) (loc2 : Asai.Range.t) : Asai.Range.t =
  let left1, right1 = Asai.Range.split loc1 in
  let left2, right2 = Asai.Range.split loc2 in
  Asai.Range.make (min_position left1 left2, max_position right1 right2)

let merge_ranges_opt loc1 loc2 =
  match loc1, loc2 with
  | Some loc1, Some loc2 -> Some (merge_ranges loc1 loc2)
  | Some loc1, None -> Some loc1
  | None, Some loc2 -> Some loc2
  | None, None -> None

let locate_opts locs =
  let merged = List.fold_left merge_ranges_opt None locs in
  Asai.Range.locate_opt merged