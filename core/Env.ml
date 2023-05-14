
include Map.Make (String)
let pp (pp_el : Format.formatter -> 'a -> unit) : Format.formatter -> 'a t -> unit = 
  fun fmt map ->
  Format.fprintf fmt "@[{";
  begin
    map |> iter @@ fun k v ->
    Format.fprintf fmt "@[%s ~> %a@;@]" k pp_el v
  end;
  Format.fprintf fmt "}@]"
