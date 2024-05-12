open Ast
open Arithmetic
open Trigonometry
open Probability
open Num
open Math

let int_or_float res =
  if float_of_int (int_of_float res) <> res then Float res
  else Int (int_of_float res)

let eval_float_binop bop e1 e2 =
  match bop with
  | Add -> int_or_float (add_f e1 e2)
  | Sub -> int_or_float (sub_f e1 e2)
  | Mult -> int_or_float (mult_f e1 e2)
  | Div -> int_or_float (div_f e1 e2)
  | Perm -> int_or_float (permutation e1 e2)
  | Comb -> int_or_float (combination e1 e2)
  | Gcd -> int_or_float (float_of_int (gcd (int_of_float e1) (int_of_float e2)))
  | Remainder ->
      int_or_float
        (float_of_int (remainder (int_of_float e1) (int_of_float e2)))
  | Pow -> int_or_float (pow e1 e2)
  | Nroot -> int_or_float (n_root e1 e2)

let eval_float_uop_2 uop e1 =
  match uop with
  | Sin -> int_or_float (sin e1)
  | Cos -> int_or_float (cos e1)
  | Tan -> int_or_float (tan e1)
  | ASin -> int_or_float (asin e1)
  | ACos -> int_or_float (acos e1)
  | ATan -> int_or_float (atan e1)
  | Fact -> int_or_float (factorial e1)
  | Abs -> int_or_float (abs_f e1)
  | Round -> int_or_float (round e1)
  | Floor -> int_or_float (floor e1)
  | _ -> failwith "Not yet supported"

let eval_float_uop uop e1 =
  match uop with
  | Ln -> int_or_float (ln e1)
  | Inv -> int_or_float (inverse e1)
  | Log -> int_or_float (log e1)
  | Square -> int_or_float (square e1)
  | Cube -> int_or_float (cube e1)
  | Sqrt -> int_or_float (sqrt e1)
  | Cubrt -> int_or_float (cube_root e1)
  | TenX -> int_or_float (ten_x e1)
  | Exp -> int_or_float (exp e1)
  | _ -> eval_float_uop_2 uop e1

let rec eval_expr = function
  | Binop (bop, e1, e2) -> match_binop (bop, e1, e2)
  | Unop (uop, e1) -> match_unop (uop, e1)
  | Const c -> match_const c
  | Int i -> Int i
  | Float f -> Float f
  | _ -> failwith "Not yet supported"

and match_binop (bop, e1, e2) =
  match (e1, e2) with
  | Binop (bop', e1', e2'), en -> begin
      let r_op_ans = eval_expr (Binop (bop', e1', e2')) in
      eval_expr (Binop (bop, r_op_ans, en))
    end
  | en, Binop (bop', e1', e2') -> begin
      let l_op_ans = eval_expr (Binop (bop', e1', e2')) in
      eval_expr (Binop (bop, en, l_op_ans))
    end
  | Unop (uop', e1'), en -> begin
      let r_op_ans = eval_expr (Unop (uop', e1')) in
      eval_expr (Binop (bop, r_op_ans, en))
    end
  | en, Unop (uop', e1') -> begin
      let l_op_ans = eval_expr (Unop (uop', e1')) in
      eval_expr (Binop (bop, en, l_op_ans))
    end
  | _ -> eval_binop_single_no_rec (bop, e1, e2)

and eval_binop_single_no_rec (bop, e1, e2) =
  match (e1, e2) with
  | Float e1', Float e2' -> eval_float_binop bop e1' e2'
  | Float e1', Int e2' -> eval_float_binop bop e1' (float_of_int e2')
  | Int e1', Float e2' -> eval_float_binop bop (float_of_int e1') e2'
  | Int e1', Int e2' ->
      eval_float_binop bop (float_of_int e1') (float_of_int e2')
  | Const c, en -> eval_expr (Binop (bop, eval_expr (Const c), en))
  | en, Const c -> eval_expr (Binop (bop, en, eval_expr (Const c)))
  | _ -> failwith "Not yet supported"

and match_unop (uop, e1) =
  match e1 with
  | Int e1' -> eval_float_uop uop (float_of_int e1')
  | Float e1' -> eval_float_uop uop e1'
  | Const c -> begin
      let op_ans = eval_expr (Const c) in
      eval_expr (Unop (uop, op_ans))
    end
  | Unop (uop', e1') -> begin
      let op_ans = eval_expr (Unop (uop', e1')) in
      eval_expr (Unop (uop, op_ans))
    end
  | Binop (bop', e1', e2') -> begin
      let op_ans = eval_expr (Binop (bop', e1', e2')) in
      eval_expr (Unop (uop, op_ans))
    end
  | _ -> failwith "Not yet supported"

and match_const = function
  | s when s = "pi" -> Float pi
  | s when s = "e" -> Float e
  | _ -> failwith "Not yet supported"

let expr_to_string = function
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | _ -> failwith "Not yet supported"

let frac_to_string (n, d) = string_of_int n ^ "/" ^ string_of_int d

let eval_query (input : string) : string =
  let lexbuf = Lexing.from_string input in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with
  | Frac e -> frac_to_string (frac (eval_expr e))
  | _ -> expr_to_string (eval_expr ast)
