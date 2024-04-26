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
%token LOG
%token LN

%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left MINUS
%left TIMES
%left DIVIDE
%left INV
%left SQRT
%left LOG
%left LN

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
  | INV; e1 = expr {Unop (Inv, e1)}
  | SQRT; e1 = expr {Unop (Sqrt, e1)}
  | LOG; e1 = expr {Unop (Log, e1)}
  | LN; e1 = expr {Unop (Ln, e1)}
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  ;

(* Trailer *)