open Final_project.Help
open Final_project.Ast
open Final_project.Arithmetic
open Plplot

let display_menu () =
  print_endline "Welcome to the ArduinBro's Calculator!";
  print_endline "";
  print_endline "";
  print_endline "Below are the current commands offered:";
  print_endline "| plot: Plot a function you want |";
  print_endline
    "| eval: Basic calculations and operations offered by the calculator |";
  print_endline
    "| settings: Change certain features of the calculator when solving \
     problems |";
  print_endline "| --help: If you are not sure how to use any features |";
  print_endline "| quit: Quit calculator program | ";
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

let rec eval_expr = function
  | Binop (bop, e1, e2) -> begin
      (* Binop case *)
      match (e1, e2) with
      | Float e1', Int e2' -> eval_float_binop bop e1' (float_of_int e2')
      (* Float and Int exprs *)
      | Int e1', Float e2' -> eval_float_binop bop (float_of_int e1') e2'
      (* Int and Float exprs *)
      | Int e1', Int e2' -> eval_int_binop bop e1' e2'
      (* Int and Int exprs *)
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
      | _ -> failwith "not yet supported"
    end
  | Unop (uop, e1) -> begin
      match uop with
      | Ln -> Float (ln (eval_expr e1))
      | _ -> failwith "not yet supported"
    end
  | _ -> failwith "not yet supported"

let rec parse_string () =
  let _ = print_endline "Enter evaluation: " in
  let input = read_line () in
  if input = "quit" then ()
  else
    let lexbuf = Lexing.from_string input in
    let ast = Final_project.Parser.prog Final_project.Lexer.read lexbuf in
    match eval_expr ast with
    | Int i ->
        print_endline (string_of_int i);
        parse_string ()
    | Float f ->
        print_endline (string_of_float f);
        parse_string ()
    | _ -> failwith "not yet supported"

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
  let _ = print_endline "Enter function to plot: " in
  let input = read_line () in
  if input = "quit" then ()
  else
    let f x =
      let input_func = input ^ "(" ^ string_of_float x ^ ")" in
      let lexbuf = Lexing.from_string input_func in
      try
        let ast = Final_project.Parser.prog Final_project.Lexer.read lexbuf in
        match ast with
        | Int i -> float_of_int i
        | Float f -> f
        | _ -> failwith "e"
      with Final_project.Parser.Error -> float_of_string input
    in
    plinit ();
    plenv xmin xmax ymin ymax 0 0;
    pllab x_label y_label title;
    plline [| xmin; xmax |] [| f xmin; f xmax |];
    plend ();
    display_menu ()

let rec process_input () =
  print_endline "Enter a command:";
  match read_line () with
  | "quit" -> print_endline "Exiting calculator."
  | "--help" ->
      print_endline
        "Enter the section you need help with (plot, eval, settings):";
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
