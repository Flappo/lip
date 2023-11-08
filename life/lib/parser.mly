%{
    (* range con estremi inclusi *)
    let rec to_range a b =
    if a > b 
        then [] 
        else a::to_range (a+1) b 

%}

(* TOKENS *)
%token E
%token S
%token B
%token COMMA
%token SLASH
%token RANGE
%token <string> CONST

(* Grammar start *)
%start <Rule.rule> rule

(* Precedenze *)
%%

(* Regole *)
rule:
(* Come si compone quello che vogliamo in input *)
| E; option(S); l1 = separated_list(COMMA, elem); SLASH; option(B); l2 = separated_list(COMMA, elem); EOF { (List.flatten l1, List.flatten l2)}
(* separated_list crea liste, flatten rende una lista con elementi listati, 
con flatten rendo quella lista di elementi listati una lista di elementi e basta 
List.flatten [[1;2];[3]] => [1;2;3] *)


range:
| a = num; RANGE; b = num { to_range a b } 

num:
| n = CONST { int_of_string n }

elem:
| n = num { [ n ] }
| ns = range { ns }