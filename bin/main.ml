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

let rec eval_expr ast =
  match ast with
  | Binop (bop, e1, e2) -> begin
      match bop with
      | Add -> add_i (eval_expr e1) (eval_expr e2)
      | Sub -> sub_i (eval_expr e1) (eval_expr e2)
      | Mult -> mult_i (eval_expr e1) (eval_expr e2)
      | Div -> div_i (eval_expr e1) (eval_expr e2)
      | _ -> 0
    end
  | Int i -> i
  | _ -> 0

let eval_query (input : string) : string =
  let lexbuf = Lexing.from_string input in
  let ast = Final_project.Parser.prog Final_project.Lexer.read lexbuf in
  string_of_int (eval_expr ast)

let rec parse_string () =
  let _ = print_endline "Enter evaluation: " in
  let input = read_line () in
  if input = "quit" then ()
  else
    let _ = print_endline (eval_query input) in
    parse_string ()

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
        | _ -> float_of_int (eval_expr ast)
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
