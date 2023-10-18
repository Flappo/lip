open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int option


let int_of_Some x = match x with 
| Some y -> y
| None -> failwith "error"

(* FARE IN MODO CHE SPACCHETTI UN OPTION *)
let string_of_result n = match n with
| None -> failwith "error"
| Some m -> string_of_int m

(* eval : ast -> result *)
let rec eval = function
    Const(n) -> Some n
  | Add(e1,e2) -> Some (int_of_Some(eval e1) + int_of_Some(eval e2))
  | Sub(e1,e2) -> Some (int_of_Some(eval e1) - int_of_Some(eval e2)) (* sottrazione *)
  | Mul(e1,e2) -> Some (int_of_Some(eval e1) * int_of_Some(eval e2)) (* moltiplicazione *)
  | Div(e1,e2) -> if (int_of_Some(eval e2)) = 0 
                  then
                    None
                  else
                    Some (int_of_Some(eval e1) / int_of_Some(eval e2)) (* divisione *)