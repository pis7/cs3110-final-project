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
  | "/" { DIVIDE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "inv" { INV }
  | "sqrt" { SQRT }
  | "log" { LOG }
  | "ln" { LN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }