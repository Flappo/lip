(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let toklist_of_string s =
  if s = ""
    then failwith "error"
else (
  let explosion = explode s
in toklist_of_string s
  let rec toklist_of_string_explode explosion = match explosion with
  | 'A'::tl -> A::toklist_of_string_explode tl
  | 'B'::tl -> B::toklist_of_string_explode tl
  | '='::tl -> X::toklist_of_string_explode tl
  | [] -> []
  | _::_ -> failwith "error"
in toklist_of_string explosion
)

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = match l with
| B::_ | [] -> false
| A::B::_ -> false
| A::tl | X::tl -> let rec validA tl = match tl with
           | B::_ | [] -> false
           | A::tl -> validA tl
           | X::tl -> let rec validX tl = match tl with
                      | B::[] | [] -> true
                      | A::_ -> false
                      | X::tl -> validX tl
                      | B::tl -> validX tl
           in validX tl
in validA tl


(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let rec counterA l = match l with
| [] | X::_ | B::_ -> 0
| A::tl -> 1 + counterA tl

let rec counterB l = match l with
| A::tl | X::tl -> 0 + counterB tl
| B::tl -> 1 + counterB tl
| [] -> 0
let win l = if (counterA l > counterB l) 
              then A
            else if (counterA l < counterB l) 
              then B
            else X

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
| A -> "the winner is A!"
| B -> "the winner is B!"
| X -> "there's a tie!" 