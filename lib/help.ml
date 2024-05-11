let plot_help () =
  ANSITerminal.(
    print_string [ blue ]
      "-----------------------------------------------------------------------\n");
  print_endline "The plot command supports the following operations:";
  ANSITerminal.(
    print_string [ green ]
      "When prompted for: (x label, y label, plot title) input the values as a \
       string. Example: Time, Temperature, ...\n");
  ANSITerminal.(
    print_string [ green ]
      "When prompted for: (lower x bound, upper x bound, lower y bound, upper \
       y bound, and function to plot) input the values as a \
       floats/ints/supported eval. Example: 1.2, 2, 2 + 4.3\n");
  ANSITerminal.(
    print_string [ green ]
      "Supported Evals Include: 'sin', 'cos', 'tan', '+', '-', '*', '/'\n");
  ANSITerminal.(
    print_string [ green ]
      "When prompted for: (Enter device number or keyword:) Choose the file \
       you would like to output it as. Example: 8 (Note, this will output a \
       pdf. Other options are displayed when on this part)\n");
  ANSITerminal.(
    print_string [ green ]
      "When prompted for: (Enter graphics output file name:) Name the file you \
       would like with its extension. Example: <name>.<extension you chose \
       above>\n");
  ANSITerminal.(
    print_string [ blue ]
      "-----------------------------------------------------------------------\n")

let eval_help () =
  ANSITerminal.(
    print_string [ blue ]
      "-----------------------------------------------------------------------\n");
  print_endline "The eval command supports the following operations:";
  ANSITerminal.(
    print_string [ green ] "Add (Addition): Use '+' Example: 2 + 2\n");
  ANSITerminal.(
    print_string [ green ] "Sub (Subtraction): Use '-' Example: 2 - 2\n");
  ANSITerminal.(
    print_string [ green ] "Mult (Multiplication): Use '*' Example: 2 * 2\n");
  ANSITerminal.(
    print_string [ green ] "Div (Division): Use '/' Example: 2 / 2\n");
  ANSITerminal.(
    print_string [ green ] "Perm (Permutation): Use 'perm' Example: perm 4 2\n");
  ANSITerminal.(
    print_string [ green ] "Comb (Combination): Use 'comb' Example: comb 4 2\n");
  ANSITerminal.(
    print_string [ green ]
      "Gcd (Greatest Common Divisor): Use 'gcd' Example: gcd 40 4\n");
  ANSITerminal.(
    print_string [ green ]
      "Remainder (Mod): Use 'remainder' Example: remainder 7 4\n");
  ANSITerminal.(
    print_string [ green ] "Pow (Power): Use 'pow' Example: pow 2 4\n");
  ANSITerminal.(
    print_string [ green ] "Nroot (N-th root): Use 'nroot' Example: nroot 8 3\n");
  ANSITerminal.(
    print_string [ green ] "Inv (Inverse): Use 'inv' Example: inv(2)\n");
  ANSITerminal.(
    print_string [ green ] "Square: Use 'square' Example: square(2)\n");
  ANSITerminal.(print_string [ green ] "Cube: Use 'cube' Example: cube(2)\n");
  ANSITerminal.(
    print_string [ green ] "Sqrt (Square root): Use 'sqrt' Example: sqrt(4)\n");
  ANSITerminal.(
    print_string [ green ]
      "Log (Logarithm Base 10): Use 'log' Example: log(100)\n");
  ANSITerminal.(
    print_string [ green ] "Ln (Natural Logarithm): Use 'ln' Example: ln(e)\n");
  ANSITerminal.(
    print_string [ green ] "TenX (10 To The x): Use 'tenx' Example: tenx 2\n");
  ANSITerminal.(
    print_string [ green ] "Exp (Exponential): Use 'exp' Example: exp(1)\n");
  ANSITerminal.(
    print_string [ green ] "Sin: (Sine): Use 'sin' Example: sin(pi/2)\n");
  ANSITerminal.(
    print_string [ green ] "Cos (Cosine): Use 'cos' Example: cos(pi/2)\n");
  ANSITerminal.(
    print_string [ green ] "Tan (Tangent): Use 'tan' Example: tan(pi/4)\n");
  ANSITerminal.(
    print_string [ green ] "ASin (Arcsine): Use 'asin' Example: asin(1)\n");
  ANSITerminal.(
    print_string [ green ] "ACos (Arccosine): Use 'acos' Example: acos(1)\n");
  ANSITerminal.(
    print_string [ green ] "ATan (Arctangent): Use 'atan' Example: atan(1)\n");
  ANSITerminal.(
    print_string [ green ] "Fact (Factorial): Use 'fact' Example: fact 5\n");
  ANSITerminal.(
    print_string [ green ] "Abs (Absolute): Use 'abs' Example: abs(-2)\n");
  ANSITerminal.(
    print_string [ green ] "Round: Use 'round' Example: round(2.5)\n");
  ANSITerminal.(
    print_string [ green ] "Floor: Use 'floor' Example: floor(2.5)\n");
  ANSITerminal.(
    print_string [ blue ]
      "-----------------------------------------------------------------------\n")

let invalid_option () =
  ANSITerminal.(print_string [ red ] "Enter a valid option: <plot>, <eval>\n")

let help_menu (section : string) =
  match section with
  | "plot" -> plot_help ()
  | "eval" -> eval_help ()
  | _ -> invalid_option ()
