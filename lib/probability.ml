let () = Random.self_init ()

let rec rand_tr n acc =
  match n with
  | 0 -> acc
  | _ -> rand_tr (n - 1) (Random.float 1. :: acc)

let rand n = rand_tr (int_of_float n) []

let rec fact_tr n acc =
  match n with
  | 0 -> acc
  | _ -> fact_tr (n - 1) (n * acc)

let factorial n = float_of_int (fact_tr (int_of_float n) 1)

let permutation n r =
  if n <= 0. || r <= 0. || n < r then failwith "Invalid Argument"
  else factorial n /. factorial (n -. r)

let combination n r =
  if n <= 0. || r <= 0. || n < r then failwith "Invalid Argument"
  else factorial n /. (factorial (n -. r) *. factorial r)
