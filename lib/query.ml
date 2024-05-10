open Ast
open Arithmetic
open Probability

let eval_float_binop bop e1 e2 =
  match bop with
  | Add -> Float (add_f e1 e2)
  | Sub -> Float (sub_f e1 e2)
  | Mult -> Float (mult_f e1 e2)
  | Div -> Float (div_f e1 e2)
  | Perm -> Float (permutation e1 e2)
  | Comb -> Float (combination e1 e2)
  | _ -> failwith "not yet supported"

let eval_int_binop bop e1 e2 =
  match bop with
  | Add -> Int (add_i e1 e2)
  | Sub -> Int (sub_i e1 e2)
  | Mult -> Int (mult_i e1 e2)
  | Div -> Float (div_f (e1 |> float_of_int) (e2 |> float_of_int))
  | Perm -> Int (int_of_float (permutation (float_of_int e1) (float_of_int e2)))
  | Comb -> Int (int_of_float (combination (float_of_int e1) (float_of_int e2)))
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
  | _ -> failwith "Not yet supported"

let rec eval_expr = function
  | Binop (bop, e1, e2) -> begin
      (* Binop case *)
      match (e1, e2) with
      | Float e1', Float e2' -> eval_float_binop bop e1' e2'
      (* Float and Float exprs *)
      | Float e1', Int e2' -> eval_float_binop bop e1' (float_of_int e2')
      (* Float and Int exprs *)
      | Int e1', Float e2' -> eval_float_binop bop (float_of_int e1') e2'
      (* Int and Float exprs *)
      | Int e1', Int e2' -> eval_int_binop bop e1' e2'
      (* Int and Int exprs *)
      | Const c, en -> begin
          (* Const and _ exprs *)
          let r_op_ans = eval_expr (Const c) in
          eval_expr (Binop (bop, r_op_ans, en))
        end
      | en, Const c -> begin
          (* _ and Const exprs *)
          let l_op_ans = eval_expr (Const c) in
          eval_expr (Binop (bop, en, l_op_ans))
        end
      | Binop (bop', e1', e2'), en -> begin
          (* Binop and _ exprs *)
          let r_op_ans = eval_expr (Binop (bop', e1', e2')) in
          eval_expr (Binop (bop, r_op_ans, en))
        end
      | en, Binop (bop', e1', e2') -> begin
          (* _ and Binop exprs *)
          let l_op_ans = eval_expr (Binop (bop', e1', e2')) in
          eval_expr (Binop (bop, en, l_op_ans))
        end
      | Unop (uop', e1'), en -> begin
          (* Unop and _ exprs *)
          let r_op_ans = eval_expr (Unop (uop', e1')) in
          eval_expr (Binop (bop, r_op_ans, en))
        end
      | en, Unop (uop', e1') -> begin
          (* _ and Unop exprs *)
          let l_op_ans = eval_expr (Unop (uop', e1')) in
          eval_expr (Binop (bop, en, l_op_ans))
        end
      | _ -> failwith "Not yet supported"
    end
  | Unop (uop, e1) -> begin
      match e1 with
      | Int e1' -> eval_int_uop uop e1'
      (* Int expr *)
      | Float e1' -> eval_float_uop uop e1'
      (* Float expr *)
      | Const c -> begin
          (* Const expr *)
          let op_ans = eval_expr (Const c) in
          eval_expr (Unop (uop, op_ans))
        end
      | Unop (uop', e1') -> begin
          (* Unop expr *)
          let op_ans = eval_expr (Unop (uop', e1')) in
          eval_expr (Unop (uop, op_ans))
        end
      | Binop (bop', e1', e2') -> begin
          (* Binop expr *)
          let op_ans = eval_expr (Binop (bop', e1', e2')) in
          eval_expr (Unop (uop, op_ans))
        end
      | _ -> failwith "Not yet supported"
    end
  | Const c -> begin
      (* Supported constants *)
      match c with
      | s when s = "pi" -> Float pi
      | s when s = "e" -> Float e
      | _ -> failwith "Not yet supported"
    end
  | _ -> failwith "Not yet supported"

let expr_to_string = function
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | _ -> failwith "Not yet supported"

let eval_query (input : string) : string =
  let lexbuf = Lexing.from_string input in
  let ast = Parser.prog Lexer.read lexbuf in
  expr_to_string (eval_expr ast)
