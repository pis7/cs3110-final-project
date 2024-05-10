(* Header *)
{
open Parser
}

(* Identifiers *)
let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'?digit*'.'digit*
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let const = letter+letter*

(* Rules *)
rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "perm" { PERM }
  | "comb" { COMB }
  | "pow" { POW }
  | "/" { DIVIDE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "inv" { INV }
  | "square" { SQUARE }
  | "cube" { CUBE }
  | "sqrt" { SQRT }
  | "cubrt" { CUBRT }
  | "nroot" { NROOT }
  | "log" { LOG }
  | "ln" { LN }
  | "sin" { SIN }
  | "cos" { COS }
  | "tan" { TAN }
  | "asin" { ASIN }
  | "acos" { ACOS }
  | "atan" { ATAN }
  | "fact" { FACT }
  | "gcd" { GCD }
  | "abs" { ABS }
  | "tenx" { TENX }
  | "exp" { EXP }
  | "round" { ROUND }
  | "floor" { FLOOR }
  | "remainder" { REMAINDER }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | const { CONST (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }