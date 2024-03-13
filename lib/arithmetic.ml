(** [add_i a b] is [a] added to [b] *)
let add_i a b = a + b

let%test "add_i" =
  3 = add_i 1 2

(** [sub_i a b] is [a] - [b] *)
let sub_i a b = a - b

let%test "sub_i" =
  ~-1 = sub_i 1 2

(** [mult_i a b] is [a] * [b]*)
let mult_i a b = a * b

let%test "mult_i" =
  6 = mult_i 3 2

(** [div_i a b] is integer division [a] / [b]*)
let div_i a b = a / b

let%test "div_i" = 2 = div_i 6 3