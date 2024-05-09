val eval_expr : Ast.expr -> Ast.expr
(** [eval_expr expr] evaluates the given [expr] to a simpler expression that can
    be represented by a single float or int. *)

val expr_to_string : Ast.expr -> string
(** [expr_to_string] evaluates the given [expr] to a string representation.
    Requires [expr] is a single Float or Int type. *)
