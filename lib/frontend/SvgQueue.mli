module type S =
sig
  val enqueue : name:string -> packages:string list -> source:string -> unit
  val process : unit -> unit
end

module Make (_ : sig val size : int end) : S
