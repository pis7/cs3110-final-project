open Final_project.Help
open Final_project.Ast
open Final_project.Arithmetic


let display_menu () =
  print_endline "Welcome to the ArduinBro's Calculator!";
  print_endline "";
  print_endline "";
  print_endline "Below are the current Modules offered:";
  print_endline "| Plot: Plot a function you want! |";
  print_endline
    "| Eval: Basic calculations and operations offered by the calculator |";
  print_endline
    "| Settings: Change certain features of the calculator when solving \
     problems |";
  print_endline
    "| Help: If you are not sure how to use any features, do --help |";
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

let rec parse_string () = 
  let _ = print_endline "Enter evaluation: " in
  let input = read_line () in
  if input = "quit" then () else 
  let lexbuf = Lexing.from_string (input) in
  let ast = Final_project.Parser.prog Final_project.Lexer.read lexbuf in
  let _ = print_endline (string_of_int (eval_expr ast)) in
  parse_string ()
  
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
  | "eval" -> let _ = parse_string () in process_input ()
  | _ -> process_input ()

let main () =
  display_menu ();
  process_input ()

let () = main ()


