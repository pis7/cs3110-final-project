open Final_project.Help

(* open Final_project.Ast *)
(* open Final_project.Arithmetic *)
open Final_project.Query
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
