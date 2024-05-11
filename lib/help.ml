let plot_help () = print_endline "more to come"
let eval_help () = print_endline "more to come"
let settings_help () = print_endline "more to come"

let invalid_option () =
  ANSITerminal.(
    print_string [ red ] "Enter a valid option: <plot>, <eval>, <settings>\n")

let help_menu (section : string) =
  match section with
  | "plot" -> plot_help ()
  | "eval" -> eval_help ()
  | "settings" -> settings_help ()
  | _ -> invalid_option ()
