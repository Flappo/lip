
%token <int> INT "42"
%token <string> ID "x"
%token TRUE "true" FALSE "false"
%token PLUS "+" MINUS "-" TIMES "*"
%token NOT "not" AND "and" OR "or" EQ "=" LEQ "<="
%token ASSIGN ":="
%token IF "if" THEN "then" ELSE "else"
%token WHILE "while" DO "do"
%token SEMICOLON ";"
%token LPAREN "(" RPAREN ")"
%token SKIP "skip"
%token EOF

%start <Ast.cmd> main
%{ open Ast %}

%left ";"
%nonassoc "else" "do"

%%

(* Most precedences are encoded in a stratified grammar. *)

let main :=
  ~ = cmd; EOF; <>

let cmd :=
| "skip"; { Skip }
| "while"; ~ = expr; "do"; ~ = cmd; <While>
| "if"; e = expr; "then"; c1 = cmd; "else"; c2 = cmd; { If (e,c1,c2) }
| ~ = identifier; ":="; ~ = expr; <Assign>
| "("; ~ = cmd; ")"; <>
| c1 = cmd; ";"; c2 = cmd; { Seq (c1,c2) }

let expr == logical_or_expr

let logical_or_expr :=
| logical_and_expr
| ~ = logical_or_expr; "or"; ~ = logical_and_expr; <Or>

let logical_and_expr :=
| logical_not_expr
| ~ = logical_and_expr; "and"; ~ = logical_not_expr; <And>

let logical_not_expr :=
| equality_expr
| "not"; ~ = logical_not_expr; <Not>

let equality_expr :=
| additive_expr
| ~ = equality_expr; "="; ~ = additive_expr; <Eq>
| ~ = equality_expr; "<="; ~ = additive_expr; <Leq>

let additive_expr :=
| multiplicative_expr
| ~ = additive_expr; "+"; ~ = multiplicative_expr; <Add>
| ~ = additive_expr; "-"; ~ = multiplicative_expr; <Sub>

let multiplicative_expr :=
| atomic_expr
| ~ = multiplicative_expr; "*"; ~ = atomic_expr; <Mul>

let atomic_expr :=
| constant
| ~ = identifier; <Var>
| "("; ~ = expr; ")"; <>

let constant :=
| "true"; { True }
| "false"; { False }
| ~ = "42"; <Const>

let identifier :=
| ~ = "x"; <>
