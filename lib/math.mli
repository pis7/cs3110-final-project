val frac : float -> int * int
(** [frac number] converts floating point [number] to relatively prime tuple
    representing fractional form in (numerator, denominator) to be interpreted
    as numerator / denominator for a result within 0.1 of original [number].
    Prerequisite: [number] can be represented in decimal format and -1e10 <=
    [number] <= 1e10. *)

val dec : int * int -> float
(** [dec (num, denom)] is [num] / [denom]*)

val cube : float -> float
(** [cube x] is [x]^3*)

val cube_root : float -> float
(** [cube_root x] is [x]^(1/3)*)

val n_root : float -> float -> float
(** [n_root n x] is [x]^(1/[n]) *)
