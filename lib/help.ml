let help_menu (section : string) =
  match section with
  | "plot" -> print_endline "more to come"
  | "eval" -> print_endline "more to come"
  | "settings" -> print_endline "more to come"
  | _ ->
      ANSITerminal.(
        print_string [ red ]
          "Enter a valid option: <plot>, <eval>, <settings>\n")
