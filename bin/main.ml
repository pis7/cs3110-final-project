open Final_project.Help
open Final_project.Ast
open Final_project.Arithmetic
open Plplot

let display_menu () =
  ANSITerminal.(print_string [ red ] "Welcome to the ArduinBro's Calculator!\n");
  print_endline "";
  let line = String.make 80 '-' in
  ANSITerminal.(print_string [ blue ] (line ^ "\n"));
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " Below are the current commands \
       offered:                                      ");
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " plot: Plot a function you \
       want                                               ");
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " eval: Basic calculations and operations offered by the \
       calculator            ");
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " settings: Change certain features of the calculator when solving \
       problems    ");
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " --help: If you are not sure how to use any \
       features                          ");
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " quit: Quit calculator \
       program                                                ");
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] (line ^ "\n"));
  print_endline "\nBegin:"

let eval_float_binop bop e1 e2 =
  match bop with
  | Add -> Float (add_f e1 e2)
  | Sub -> Float (sub_f e1 e2)
  | Mult -> Float (mult_f e1 e2)
  | Div -> Float (div_f e1 e2)
  | _ -> failwith "not yet supported"

let eval_int_binop bop e1 e2 =
  match bop with
  | Add -> Int (add_i e1 e2)
  | Sub -> Int (sub_i e1 e2)
  | Mult -> Int (mult_i e1 e2)
  | Div -> Int (div_i e1 e2)
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
  | _ -> failwith "Not yet supported"

let eval_int_uop uop e1 =
  match uop with
  | Ln -> Int (int_of_float (ln (float_of_int e1)))
  | Inv -> Int (int_of_float (inverse (float_of_int e1)))
  | Log -> Int (int_of_float (log (float_of_int e1)))
  | TenX -> Int (int_of_float (ten_x (float_of_int e1)))
  | Exp -> Int (int_of_float (exp (float_of_int e1)))
  | Sin -> Int (int_of_float (sin (float_of_int e1)))
  | Cos -> Int (int_of_float (cos (float_of_int e1)))
  | Tan -> Int (int_of_float (tan (float_of_int e1)))
  | ASin -> Int (int_of_float (asin (float_of_int e1)))
  | ACos -> Int (int_of_float (acos (float_of_int e1)))
  | ATan -> Int (int_of_float (atan (float_of_int e1)))
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
  let ast = Final_project.Parser.prog Final_project.Lexer.read lexbuf in
  expr_to_string (eval_expr ast)

let rec parse_string () =
  let _ = print_endline "Enter evaluation: " in
  let input = read_line () in
  if input = "quit" then ()
  else
    let _ = print_endline (eval_query input) in
    parse_string ()

(** Saved this [Plot_string] implementation for later so we can refer to it*)
(* let plot_string () = let _ = print_endline "Enter x label: " in let x_label =
   read_line () in let _ = print_endline "Enter y label: " in let y_label =
   read_line () in let _ = print_endline "Enter plot title: " in let title =
   read_line () in let _ = print_endline "Enter lower x bound: " in let xmin =
   float_of_string (read_line ()) in let _ = print_endline "Enter upper x bound:
   " in let xmax = float_of_string (read_line ()) in let _ = print_endline
   "Enter lower y bound: " in let ymin = float_of_string (read_line ()) in let _
   = print_endline "Enter upper y bound: " in let ymax = float_of_string
   (read_line ()) in let _ = print_endline "Enter function to plot: " in let
   input = read_line () in if input = "quit" then () else let f x = let
   input_func = input ^ "(" ^ string_of_float x ^ ")" in let lexbuf =
   Lexing.from_string input_func in try let ast = Final_project.Parser.prog
   Final_project.Lexer.read lexbuf in match ast with | Int i -> float_of_int i |
   _ -> float_of_int (eval_expr ast) with Final_project.Parser.Error ->
   float_of_string input in plinit (); plenv xmin xmax ymin ymax 0 0; pllab
   x_label y_label title; plline [| xmin; xmax |] [| f xmin; f xmax |]; plend
   (); display_menu () *)

let plot_string () =
  Quick_plot.func
    ~labels:("Angle (radians)", "f(x)", "Sin and Cos Functions")
    ~names:[ "sin"; "cos" ] [ sin; cos ] (-3.15, 3.15);
  display_menu ()

let rec process_input () =
  print_endline "Enter a command:";
  match read_line () with
  | "quit" -> ANSITerminal.(print_string [ red ] "Exiting calculator.\n")
  | "--help" ->
      ANSITerminal.(
        print_string [ cyan ]
          "Enter the section you need help with (plot, eval, settings):\n");
      let section = read_line () in
      help_menu section;
      process_input ()
  | "eval" ->
      let _ = parse_string () in
      process_input ()
  | "plot" ->
      let _ = plot_string () in
      process_input ()
  | _ -> process_input ()

let main () =
  display_menu ();
  process_input ()

let () = main ()
