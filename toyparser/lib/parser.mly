%{
open Ast
%}

%token <string> CONST
%token PLUS
%token SUB (* sottrazione *)
%token MUL (* moltiplicazione *)
%token DIV (* divisione *)
%token LPAREN
%token RPAREN
%token EOF


(* le operazioni più in basso hanno una maggiore priorità *)
%left SUB (* sottrazione *)
%left PLUS
%left DIV (* divisione *)
%left MUL (* moltiplicazione *)


%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) } (* int_of_string rende interi sia i valori numerici che esadecimali *)
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) } (* sottrazione *)
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) } (* moltiplicazione *)
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) } (* divisione *)
  | MINUS, e1 = expr { Sub(0,e) } (* sottrazione unitaria *)
  | LPAREN; e=expr; RPAREN {e}
;