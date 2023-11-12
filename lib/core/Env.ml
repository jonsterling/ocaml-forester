include Map.Make (Symbol)

let pp (pp_el : Format.formatter -> 'a -> unit) : Format.formatter -> 'a t -> unit =
  fun fmt map ->
  Format.fprintf fmt "@[<v1>{";
  begin
    map |> iter @@ fun k v ->
    Format.fprintf fmt "@[%a ~> %a@]@;" Symbol.pp k pp_el v
  end;
  Format.fprintf fmt "}@]"
