(* Header *)
%{
open Ast
%}

(* Declarations *)
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> CONST
%token TRUE
%token FALSE
%token LEQ
%token TIMES
%token DIVIDE
%token PLUS
%token MINUS
%token PERM
%token COMB
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF
%token INV
%token SQRT
%token SIN
%token COS
%token TAN
%token ASIN
%token ACOS
%token ATAN
%token LOG
%token LN
%token FACT
%token GCD
%token ABS
%token ROUND
%token FLOOR
%token REMAINDER

%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left MINUS
%left TIMES
%left DIVIDE
%left PERM
%left COMB
%left INV
%left SQRT
%left SIN
%left COS
%left TAN
%left ASIN
%left ACOS
%left ATAN
%left LOG
%left LN
%left FACT
%left GCD
%left ABS
%left ROUND
%left FLOOR
%left REMAINDER

%start <Ast.expr> prog (* start with rule called prog which will return an AST.expr *)

%%

(* Rules *)

prog:
  | e = expr; EOF { e }
  ;

expr:
  | c = CONST { Const c }
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; DIVIDE; e2 = expr { Binop (Div, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) }
  | PERM; e1 = expr; e2 = expr {Binop (Perm, e1, e2) }
  | COMB; e1 = expr; e2 = expr {Binop (Comb, e1, e2) }
  | GCD; e1 = expr; e2 = expr { Binop (Gcd, e1, e2) }
  | REMAINDER; e1 = expr; e2 = expr { Binop (Remainder, e1, e2) }
  | INV; e1 = expr {Unop (Inv, e1) }
  | SQRT; e1 = expr {Unop (Sqrt, e1) }
  | LOG; e1 = expr {Unop (Log, e1) }
  | LN; e1 = expr {Unop (Ln, e1) }
  | SIN; e1 = expr {Unop (Sin, e1) }
  | COS; e1 = expr {Unop (Cos, e1) }
  | TAN; e1 = expr {Unop (Tan, e1) }
  | ASIN; e1 = expr {Unop (ASin, e1) }
  | ACOS; e1 = expr {Unop (ACos, e1) }
  | ATAN; e1 = expr {Unop (ATan, e1) }
  | FACT; e1 = expr {Unop (Fact, e1) }
  | ABS; e1 = expr {Unop (Abs, e1) }
  | ROUND; e1 = expr {Unop (Round, e1) }
  | FLOOR; e1 = expr {Unop (Floor, e1) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  ;

(* Trailer *)