let rec gcd a b =
  if b = 0 then a
  else
    let r = a mod b in
    gcd b r
(* Adapted from https://www.baeldung.com/cs/euclid-time-complexity, Section
   4.1-4.2, accessed 1/31/24. To implement Euclidean's Algorithm by division
   from pseudo code*)

let abs_i x = if x > 0 then x else ~-x
let abs_f x = if x > 0. then x else ~-.x
let round x = Float.round x
let floor x = Float.floor x
let remainder n d = n mod d

let rec min lst =
  match lst with
  | [] -> neg_infinity
  | h :: t -> Stdlib.min h (min t)

let rec max lst =
  match lst with
  | [] -> infinity
  | h :: t -> Stdlib.max h (max t)
