let print_command_msg line =
  ANSITerminal.(print_string [ blue ] (line ^ "\n"));
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " Below are the current commands \
       offered:                                      ");
  ()

let print_plot () =
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " plot: Plot a function you \
       want                                               ");
  ()

let print_eval () =
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " eval: Basic calculations and operations offered by the \
       calculator            ");
  ()

let print_help () =
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " --help: If you are not sure how to use any \
       features                          ");
  ()

let print_quit () =
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] "|");
  ANSITerminal.(
    print_string [ green ]
      " quit: Quit calculator \
       program                                                ");
  ()

let display_menu () =
  ANSITerminal.(print_string [ red ] "Welcome to the ArduinBro's Calculator!\n");
  print_endline "";
  let line = String.make 80 '-' in
  print_command_msg line;
  print_plot ();
  print_eval ();
  print_help ();
  print_quit ();
  ANSITerminal.(print_string [ blue ] "|\n");
  ANSITerminal.(print_string [ blue ] (line ^ "\n"))
