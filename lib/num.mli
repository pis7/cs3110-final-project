val gcd : int -> int -> int
(** [gcd a b] Greatest Common Denominator of [a] and [b]. Ill defined for
    negative numbers*)

val abs : float -> float
(** [abs x] is the absolute value of [x]*)

val round : float -> float
(** [round x] is [x] rounded to the nearest whole number*)

val floor : float -> float
(** [floor x] is [x] floored to the next whole number*)

val min : float list -> float
(** [min lst] is the smallest float in [lst]*)

val max : float list -> float
(** [max lst] is the largest float in [lst]*)

val remainder : int -> int -> int
(** [remainder n d] is the remainder of [n] / [d]*)
