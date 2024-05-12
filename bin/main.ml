(** @author George Maidhof (gpm58) & Parker Schless (pis7) & Edward Liu (ejl248) *)

open Final_project.Help
open Final_project.Query
open Final_project.Menu
open Final_project.Plot

let rec parse_string () =
  let _ =
    print_endline "\nEnter evaluation (\"quit\" to go back to main menu): "
  in
  let input = read_line () in
  try
    begin
      if input = "quit" then ()
      else
        let _ = print_endline (eval_query input) in
        parse_string ()
    end
  with _ ->
    begin
      let _ = print_endline "\nInvalid expression!" in
      parse_string ()
    end

let rec process_input () =
  print_endline "\nEnter a command:";
  match read_line () with
  | "quit" -> ANSITerminal.(print_string [ red ] "Exiting calculator.\n")
  | "--help" ->
      ANSITerminal.(
        print_string [ cyan ]
          "Enter the section you need help with (plot, eval):\n");
      let section = read_line () in
      help_menu section;
      process_input ()
  | "eval" ->
      let _ = parse_string () in
      process_input ()
  | "plot" ->
      let _ = plot_string () in
      display_menu ();
      process_input ()
  | _ -> process_input ()

let main () =
  display_menu ();
  process_input ()

let () = main ()
