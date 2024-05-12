open Ast
open Plplot

let rec substitute_x_in_expr x_value expr =
  match expr with
  | Var v when v = "x" -> Float x_value
  | Var _ -> failwith "Unsupported variable. Only 'x' is supported."
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
  | _ -> failwith "Not yet supported"

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

let rec read_float_from_user prompt =
  print_endline prompt;
  let input = read_line () in
  try float_of_string input
  with Failure _ ->
    print_endline "Invalid input. Please enter a float or int.";
    read_float_from_user prompt

let rec read_int_from_user prompt =
  print_endline prompt;
  let input = read_line () in
  try int_of_string input
  with Failure _ ->
    print_endline "Invalid input. Please enter a float or int.";
    read_int_from_user prompt

let read_string_from_user prompt =
  print_endline prompt;
  read_line ()

let colors = [| 1; 2; 3 |]

let read_labels_and_bounds () =
  let x_label = read_string_from_user "Enter x label: " in
  let y_label = read_string_from_user "Enter y label: " in
  let title = read_string_from_user "Enter plot title: " in
  let xmin = read_float_from_user "Enter lower x bound: " in
  let xmax = read_float_from_user "Enter upper x bound: " in
  let ymin = read_float_from_user "Enter lower y bound: " in
  let ymax = read_float_from_user "Enter upper y bound: " in
  (x_label, y_label, title, xmin, xmax, ymin, ymax)

let read_functions num_funcs =
  let func_inputs = Array.make num_funcs "" in
  let funcs =
    Array.init num_funcs (fun i ->
        let input = read_string_from_user "Enter function to plot:" in
        if input = "quit" then failwith "Quit command received";
        func_inputs.(i) <- input;
        let ast = string_to_ast input in
        fun x_value ->
          let ast_with_x = substitute_x_in_expr x_value ast in
          print_endline (Ast.string_of_expr ast_with_x);
          try eval_expr_to_float ast_with_x x_value
          with Parser.Error -> failwith "Parsing error")
  in
  (func_inputs, funcs)

let plot_functions x_label y_label title xmin xmax ymin ymax func_inputs funcs =
  plinit ();
  plenv xmin xmax ymin ymax 0 0;
  pllab x_label y_label title;
  let x_values =
    Array.init 100 (fun i ->
        xmin +. ((xmax -. xmin) *. (float_of_int i /. 99.)))
  in
  Array.iteri
    (fun i f ->
      let y_values = Array.map f x_values in
      plcol0 colors.(i mod Array.length colors);
      plline x_values y_values;
      plmtex "t" 1.0 (0.1 +. (0.2 *. float_of_int i)) 0.5 func_inputs.(i))
    funcs;
  plend ()

let rec plot_string () =
  try
    let num_funcs =
      read_int_from_user "Enter number of functions to plot (up to 3): "
    in
    let x_label, y_label, title, xmin, xmax, ymin, ymax =
      read_labels_and_bounds ()
    in
    let func_inputs, funcs = read_functions num_funcs in
    plot_functions x_label y_label title xmin xmax ymin ymax func_inputs funcs
  with Failure msg ->
    print_endline ("THERE WAS AN ERROR!!!: " ^ msg);
    plot_string ()
