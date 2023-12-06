%{
open Ast
%}

%token <int> CONST
%token PLUS
%token SUB (* sottrazione *)
%token MUL (* moltiplicazione *)
%token DIV (* divisione *)
%token LPAREN
%token RPAREN
%token EOF

(*while exercise*)
%token SKIP
%token <bool> TRUE
%token <bool> FALSE
%token <string> VAR

%token AND
%token OR
%token NOT

%token EQ
%token LEQ
%token ASSIGN

%token IF (*if then else*)
%token THEN
%token ELSE
%token WHILE (*while do*)
%token DO 

%token SEQ (*sequenza ";"  *)



(* le operazioni più in basso hanno una maggiore priorità *)
%left SUB (* sottrazione *)
%left PLUS
%left DIV (* divisione *)
%left MUL (* moltiplicazione *)

(*while exercise*)
%nonassoc IF
%nonassoc THEN
%nonassoc ELSE
%nonassoc WHILE
%nonassoc DO

%left LEQ
%left EQ
%left OR
%left AND
%left NOT
%left ASSIGN
%left SEQ (**)



%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | n = CONST { Const( n ) } (* int_of_string rende interi sia i valori numerici che esadecimali *)
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) } (* sottrazione *)
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) } (* moltiplicazione *)
  | SUB; e = expr { Sub(Const 0,e) } (* sottrazione unitaria *)
  | LPAREN; e=expr; RPAREN {e}
(*while exercise*)
  | TRUE { True }
  | FALSE { False }
  | v = VAR { Var(v) }
  | e1 = expr; AND; e2 = expr { And(e1,e2) }
  | e1 = expr; OR; e2 = expr { Or(e1,e2) }
  | NOT; e = expr { Not(e) }
  | e1 = expr; EQ; e2 = expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1,e2) }
;

cmd:
  | SKIP { Skip }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e,c1,c2) }
  | WHILE; e = expr; DO; c = cmd { While(e,c) } 
  | v = VAR; ASSIGN; e = expr { Assign(v,e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1,c2) }
;