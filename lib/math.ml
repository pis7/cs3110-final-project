open Num
open Arithmetic
open Ast

(** [reduce_relatively_prime numerator denominator] Returns [numerator] and
    [denominator] in relatively prime tuple of form (reduced_numerator,
    reduced_denominator)*)
let reduce_relatively_prime numerator denominator =
  let factor = gcd numerator denominator in
  (numerator / factor, denominator / factor)

let frac = function
  | Int i -> begin
      let num, denom =
        reduce_relatively_prime
          (int_of_float (float_of_int i *. 100000000.))
          100000000
      in
      if denom < 0 then (~-num, ~-denom) else (num, denom)
    end
  | Float f -> begin
      let num, denom =
        reduce_relatively_prime (int_of_float (f *. 100000000.)) 100000000
      in
      if denom < 0 then (~-num, ~-denom) else (num, denom)
    end
  | _ -> failwith "Not yet supported"

let dec (num, denom) = (num |> float_of_int) /. (denom |> float_of_int)
let cube x = x ** 3.
let n_root n x = pow n (1. /. x)
let cube_root n = Float.cbrt n
