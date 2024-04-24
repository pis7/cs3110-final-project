val sin : float -> float
(** [sin theta] is sin([theta]) for [theta] in radians *)

val cos : float -> float
(** [cos theta] is cos([theta]) for [theta] in radians *)

val tan : float -> float
(** [tan theta] is tan([theta]) for [theta] in radians *)

val acos : float -> float
(** [acos x] is Arc cosine [x]. [x] must be within* [-1.0, 1.0]. Result is in
    radians between 0.0 and pi *)

val asin : float -> float
(** [asin x] is Arc sine [x]. [x] must be within* [-1.0, 1.0]. Result is in
    radians between -pi/2 and pi/2 *)

val atan : float -> float
(** [atan x] is Arc tangent [x]. Result is in radians and is between -pi/2 and
    pi/2.*)
