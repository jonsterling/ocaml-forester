module type I =
sig
  val alphabet : string
end

module type S =
sig
  val base : int
  val int_of_string : string -> int option
  val string_of_int : int -> string
end

module Make (I : I) : S =
struct
  let base = String.length I.alphabet

  let int_of_string digits =
    let rec loop sum place r =
      if r < 0 then sum else
        let x = String.get digits r in
        let digit_value = String.index I.alphabet x in
        let sum' = sum + (place * digit_value) in
        let place' = place * base in
        loop sum' place' (r - 1)
    in
    let len = String.length digits in
    match loop 0 1 (len - 1) with
    | sum -> Some sum
    | exception _ -> None

  let string_of_int n =
    let len =
      max 4 @@
      Int.succ @@ int_of_float @@ floor @@
      log (float_of_int n) /. log (float_of_int base)
    in
    let bytes = Bytes.init len @@ fun _ -> '0' in
    let rec loop r i =
      if i <= 0 then
        Bytes.unsafe_to_string bytes
      else
        let x = String.get I.alphabet (i mod base) in
        Bytes.set bytes r x;
        loop (r - 1) (i / base)
    in
    loop (len - 1) n
end

module Base36 =
  Make
    (struct
      let alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    end)
