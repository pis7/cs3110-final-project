let plot_help () = print_endline "more to come"

let eval_help () =
  print_endline "The eval command supports the following operations:";
  print_endline "Add (Addition): Use '+' Example: 2 + 2";
  print_endline "Sub (Subtraction): Use '-' Example: 2 - 2";
  print_endline "Mult (Multiplication): Use '*' Example: 2 * 2";
  print_endline "Div (Division): Use '/' Example: 2 / 2";
  print_endline "Leq (Less Than Or Equal To): Use '' Example: ";
  (*Find*)
  print_endline "Perm (Permutation): Use '' Example:";
  (*Find*)
  print_endline "Comb (Combination): Use '' Example:";
  (*Find*)
  print_endline "Gcd (Greatest Common Divisor): Use 'gcd' Example: gcd 40 4";
  print_endline "Remainder (Mod): Use '' Example: ";
  (*Find*)
  print_endline "Pow (Power): Use '' Example: ";
  (*Find*)
  print_endline "Nroot (N-th root): Use '' Example: ";
  (*Find*)
  print_endline "Inv (Inverse): Use 'inv' Example: inv(2)";
  print_endline "Square: Use 'square' Example: square(2)";
  print_endline "Cube: Use 'cube' Example: cube(2)";
  print_endline "Sqrt (Square root): Use 'sqrt' Example: sqrt(4)";
  print_endline "Cubrt (Cube Root): Use '' Example: ";
  (*Find*)
  print_endline "Log (Logarithm Base 10): Use 'log' Example: log(100)";
  print_endline "Ln (Natural Logarithm): Use 'ln' Example: ln(e)";
  print_endline "TenX (10 To The x): Use '' Example: ";
  (*Find*)
  print_endline "Exp (Exponential): Use 'exp' Example: exp(1)";
  print_endline "Sin: (Sine): Use 'sin' Example: sin(pi/2)";
  print_endline "Cos (Cosine): Use 'cos' Example: cos(pi/2)";
  print_endline "Tan (Tangent): Use 'tan' Example: tan(pi/4)";
  print_endline "ASin (Arcsine): Use 'asin' Example: asin(1)";
  print_endline "ACos (Arccosine): Use 'acos' Example: acos(1)";
  print_endline "ATan (Arctangent): Use 'atan' Example: atan(1)";
  print_endline "Fact (Factorial): Use 'fact' Example: fact 5";
  print_endline "Abs (Absolute): Use 'abs' Example: abs(-2)";
  print_endline "Round: Use 'round' Example: round(2.5)";
  print_endline "Floor: Use 'floor' Example: floor(2.5)"

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
