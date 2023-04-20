module type S =
sig
  type out 
  type t = out -> unit

  val nil : t
  val iter : ?sep:t -> ('a -> t) -> 'a list -> t
  val seq : ?sep:t -> t list -> t
end

module Kit (P : sig type out end) : S with type out = P.out =
struct 
  include P

  type t = out -> unit
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
