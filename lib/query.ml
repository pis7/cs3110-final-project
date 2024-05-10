open Ast
open Arithmetic
open Trigonometry
open Probability
open Num

let eval_float_binop bop e1 e2 =
  match bop with
  | Add -> Float (add_f e1 e2)
  | Sub -> Float (sub_f e1 e2)
  | Mult -> Float (mult_f e1 e2)
  | Div -> Float (div_f e1 e2)
  | Perm -> Float (permutation e1 e2)
  | Comb -> Float (combination e1 e2)
  | Gcd -> Float (float_of_int (gcd (int_of_float e1) (int_of_float e2)))
  | Remainder ->
      Float (float_of_int (remainder (int_of_float e1) (int_of_float e2)))
  | _ -> failwith "not yet supported"

let eval_int_binop bop e1 e2 =
  match bop with
  | Add -> Int (add_i e1 e2)
  | Sub -> Int (sub_i e1 e2)
  | Mult -> Int (mult_i e1 e2)
  | Div -> Int (div_i e1 e2)
  | Perm -> Int (int_of_float (permutation (float_of_int e1) (float_of_int e2)))
  | Comb -> Int (int_of_float (combination (float_of_int e1) (float_of_int e2)))
  | Gcd -> Int (gcd e1 e2)
  | Remainder -> Int (remainder e1 e2)
  | _ -> failwith "not yet supported"

let eval_float_uop uop e1 =
  match uop with
  | Ln -> Float (ln e1)
  | Inv -> Float (inverse e1)
  | Log -> Float (log e1)
  | TenX -> Float (ten_x e1)
  | Exp -> Float (exp e1)
  | Sin -> Float (sin e1)
  | Cos -> Float (cos e1)
  | Tan -> Float (tan e1)
  | ASin -> Float (asin e1)
  | ACos -> Float (acos e1)
  | ATan -> Float (atan e1)
  | Fact -> Float (factorial e1)
  | Abs -> Float (abs_f e1)
  | Round -> Float (round e1)
  | Floor -> Float (floor e1)
  | _ -> failwith "Not yet supported"

let eval_int_uop uop e1 =
  match uop with
  | Ln -> Float (ln (float_of_int e1))
  | Inv -> Float (inverse (float_of_int e1))
  | Log -> Float (log (float_of_int e1))
  | TenX -> Float (ten_x (float_of_int e1))
  | Exp -> Float (exp (float_of_int e1))
  | Sin -> Float (sin (float_of_int e1))
  | Cos -> Float (cos (float_of_int e1))
  | Tan -> Float (tan (float_of_int e1))
  | ASin -> Float (asin (float_of_int e1))
  | ACos -> Float (acos (float_of_int e1))
  | ATan -> Float (atan (float_of_int e1))
  | Fact -> Int (int_of_float (factorial (float_of_int e1)))
  | Abs -> Int (abs_i e1)
  | Round -> Int (int_of_float (round (float_of_int e1)))
  | Floor -> Int (int_of_float (floor (float_of_int e1)))
  | _ -> failwith "Not yet supported"

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
  | Int e1', Int e2' -> eval_int_binop bop e1' e2'
  | Const c, en -> eval_expr (Binop (bop, eval_expr (Const c), en))
  | en, Const c -> eval_expr (Binop (bop, en, eval_expr (Const c)))
  | _ -> failwith "Not yet supported"

and match_unop (uop, e1) =
  match e1 with
  | Int e1' -> eval_int_uop uop e1'
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

let eval_query (input : string) : string =
  let lexbuf = Lexing.from_string input in
  let ast = Parser.prog Lexer.read lexbuf in
  expr_to_string (eval_expr ast)
