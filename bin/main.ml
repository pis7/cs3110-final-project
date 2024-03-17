open Final_project.Help

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
  | _ -> process_input ()

let main () =
  display_menu ();
  process_input ()

let () = main ()
