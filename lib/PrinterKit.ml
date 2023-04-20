module Kit (P : sig type out end) : 
sig
  type t = P.out -> unit
  val nil : t
  val iter : ?sep:t -> ('a -> t) -> 'a list -> t
  val seq : ?sep:t -> t list -> t
end =
struct 
  type t = P.out -> unit
  let nil : t = fun _ -> () 

  let iter ?(sep = nil) printer xs : t =
    fun fmt -> 
    let n = List.length xs in
    xs |> List.iteri @@ fun i x -> 
    if not (i = 0 || i = n) then 
      sep fmt;
    printer x fmt

  let seq ?(sep = nil) ps : t = 
    iter ~sep Fun.id ps
end
