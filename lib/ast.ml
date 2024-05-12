(* AST for sample SimPL lanuage *)

type bop =
  | Add
  | Sub
  | Mult
  | Div
  | Perm
  | Comb
  | Gcd
  | Remainder
  | Pow
  | Nroot

type uop =
  | Inv
  | Square
  | Cube
  | Sqrt
  | Cubrt
  | Log
  | Ln
  | TenX
  | Exp
  | Sin
  | Cos
  | Tan
  | ASin
  | ACos
  | ATan
  | Fact
  | Abs
  | Round
  | Floor

type expr =
  | Frac of expr
  | Var of string
  | Const of string
  | Int of int
  | Float of float
  | Bool of bool
  | Binop of bop * expr * expr
  | Unop of uop * expr
  | Let of string * expr * expr
  | If of expr * expr * expr

let rec string_of_expr expr =
  match expr with
  | Frac e -> "Frac(" ^ string_of_expr e ^ ")"
  | Var v -> "Var(" ^ v ^ ")"
  | Const c -> "Const(" ^ c ^ ")"
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Binop (op, e1, e2) ->
      "Binop(" ^ string_of_bop op ^ ", " ^ string_of_expr e1 ^ ", "
      ^ string_of_expr e2 ^ ")"
  | Unop (op, e) -> "Unop(" ^ string_of_uop op ^ ", " ^ string_of_expr e ^ ")"
  | Let (x, e1, e2) ->
      "Let(" ^ x ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | If (e1, e2, e3) ->
      "If(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ", "
      ^ string_of_expr e3 ^ ")"

and string_of_bop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Perm -> "Perm"
  | Comb -> "Comb"
  | Gcd -> "Gcd"
  | Remainder -> "Remainder"
  | Pow -> "Pow"
  | Nroot -> "Nroot"

and string_of_uop = function
  | Inv -> "Inv"
  | Square -> "Square"
  | Cube -> "Cube"
  | Sqrt -> "Sqrt"
  | Cubrt -> "Cubrt"
  | Log -> "Log"
  | Ln -> "Ln"
  | TenX -> "TenX"
  | Exp -> "Exp"
  | Sin -> "Sin"
  | Cos -> "Cos"
  | Tan -> "Tan"
  | ASin -> "ASin"
  | ACos -> "ACos"
  | ATan -> "ATan"
  | Fact -> "Fact"
  | Abs -> "Abs"
  | Round -> "Round"
  | Floor -> "Floor"
