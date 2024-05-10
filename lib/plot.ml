open Ast
open Plplot

let rec substitute_x_in_expr x_value expr =
  match expr with
  | Var _ -> Float x_value
  | (Const _ | Int _ | Float _ | Bool _) as literal -> literal
  | Binop (op, e1, e2) ->
      Binop
        (op, substitute_x_in_expr x_value e1, substitute_x_in_expr x_value e2)
  | Unop (op, e) -> Unop (op, substitute_x_in_expr x_value e)
  | Let (x, e1, e2) ->
      Let (x, substitute_x_in_expr x_value e1, substitute_x_in_expr x_value e2)
  | If (e1, e2, e3) ->
      If
        ( substitute_x_in_expr x_value e1,
          substitute_x_in_expr x_value e2,
          substitute_x_in_expr x_value e3 )

let rec eval_expr_to_float expr x_value =
  match expr with
  | Int i -> float_of_int i
  | Float f -> f
  | Var "x" -> x_value
  | Binop (op, e1, e2) -> (
      let v1 = eval_expr_to_float e1 x_value in
      let v2 = eval_expr_to_float e2 x_value in
      match op with
      | Add -> v1 +. v2
      | Sub -> v1 -. v2
      | Mult -> v1 *. v2
      | Div -> v1 /. v2
      | _ -> failwith "Unsupported operator")
  | Unop (op, e) -> (
      let v = eval_expr_to_float e x_value in
      match op with
      | Sin -> sin v
      | Cos -> cos v
      | Tan -> tan v
      | _ -> failwith "Unsupported operator")
  | _ -> failwith "Unsupported expression"

let string_to_ast input =
  let lexbuf = Lexing.from_string input in
  try Parser.prog Lexer.read lexbuf
  with Parser.Error -> failwith "Syntax error"

let plot_string () =
  let _ = print_endline "Enter x label: " in
  let x_label = read_line () in
  let _ = print_endline "Enter y label: " in
  let y_label = read_line () in
  let _ = print_endline "Enter plot title: " in
  let title = read_line () in
  let _ = print_endline "Enter lower x bound: " in
  let xmin = float_of_string (read_line ()) in
  let _ = print_endline "Enter upper x bound: " in
  let xmax = float_of_string (read_line ()) in
  let _ = print_endline "Enter lower y bound: " in
  let ymin = float_of_string (read_line ()) in
  let _ = print_endline "Enter upper y bound: " in
  let ymax = float_of_string (read_line ()) in
  let _ =
    print_endline
      "Enter function to plot (without the y=, \n\
      \ use * between coefficient and variable): "
  in
  let input = read_line () in
  if input = "quit" then ()
  else
    let ast = string_to_ast input in
    let f x_value =
      let ast_with_x = substitute_x_in_expr x_value ast in
      try eval_expr_to_float ast_with_x
      with Parser.Error -> failwith "Parsing error"
    in
    plinit ();
    plenv xmin xmax ymin ymax 0 0;
    pllab x_label y_label title;
    let x_values =
      Array.init 100 (fun i ->
          xmin +. ((xmax -. xmin) *. (float_of_int i /. 99.)))
    in
    let y_values = Array.map f x_values in
    let y_values = Array.map (fun f -> f 0.0) y_values in
    plline x_values y_values;
    plend ()
