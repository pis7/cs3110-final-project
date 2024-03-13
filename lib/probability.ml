let () = Random.self_init ()
(** [rand n] is [n] random numbers in [0,1]*)
let rec rand_tr n acc = match n with 
  | 0 -> acc
  | _ -> rand_tr (n-1) (Random.float 1. :: acc)
let rand n = rand_tr n []

let rec fact_tr n acc = fact_tr (n-1) (n * acc)

(** [factorial n] is [n] factorial*)
let factorial n = fact_tr n 0

(** [permutation n r] is [n]P[r] *)
let permutation n r = factorial n / (factorial (n-r) * factorial(r))

(** *)