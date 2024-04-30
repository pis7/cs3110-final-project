open Num
(** [reduce_relatively_prime numerator denominator] Returns [numerator] and
    [denominator] in relatively prime tuple of form (reduced_numerator,
    reduced_denominator)*)
let reduce_relatively_prime numerator denominator =
  let factor = gcd numerator denominator in
  (numerator / factor, denominator / factor)



let frac number =
  reduce_relatively_prime (int_of_float (number *. 100.)) 100

let dec (num, denom) = (num |> float_of_int) /.( denom |> float_of_int)

let cube x = x ** 3.

let n_root n x = x ** (1. /. n)

let cube_root x = n_root 3. x