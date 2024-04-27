(* AST for sample SimPL lanuage *)

type bop =
  | Add
  | Sub
  | Mult
  | Div
  | Leq
  | Perm
  | Comb

type uop =
  | Inv
  | Sqrt
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

type expr =
  | Var of string
  | Const of string
  | Int of int
  | Float of float
  | Bool of bool
  | Binop of bop * expr * expr
  | Unop of uop * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
