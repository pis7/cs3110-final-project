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
