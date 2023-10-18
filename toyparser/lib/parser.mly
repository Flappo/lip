%{
open Ast
%}

(*labels*)
%token <string> CONST
%token PLUS
%token MIN (* sottrazione *)
%token LPAREN
%token RPAREN
%token EOF

(*gli operatori più in basso hanno priorità*)
%left MIN
%left PLUS

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
