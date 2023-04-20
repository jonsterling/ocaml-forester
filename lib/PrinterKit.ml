module type S0 = 
sig
  type out
  val text : string -> out -> unit
end

module type S =
sig
  include S0
  type t = out -> unit

  val nil : t
  val iter : ?sep:t -> ('a -> t) -> 'a list -> t
  val seq : ?sep:t -> t list -> t

  val trimmedText : string -> t
  val space : t
end

module Kit (P : S0) : S with type out = P.out =
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


  let trimmedText (txt : string) : t =
    let txt = String.trim txt in 
    if String.length txt > 0 then 
      text @@ txt 
    else 
      nil

  let space = text " "
end
