open Num
(** [reduce_relatively_prime numerator denominator] Returns [numerator] and
    [denominator] in relatively prime tuple of form (reduced_numerator,
    reduced_denominator)*)
let reduce_relatively_prime numerator denominator =
  let factor = gcd numerator denominator in
  (numerator / factor, denominator / factor)



let frac number =
  let num, denom = reduce_relatively_prime (int_of_float (number *. 100000000.)) 100000000 in if denom < 0 then (~-num, ~-denom) else (num, denom)

let dec (num, denom) = (num |> float_of_int) /.( denom |> float_of_int)

let cube x = x ** 3.

let n_root n x = Float.pow x (1. /. n)

let cube_root x = Float.cbrt x