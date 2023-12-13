open Ast
open Types

let parse text =
  let lexbuf = Lexing.from_string text in
  try Parser.main Lexer.token lexbuf
  with exn ->
    let pos = lexbuf.lex_curr_p
    and errstr =
      match exn with
      | Parser.Error -> "syntax error"
      | Lexer.Error -> "unexpected character"
      | _ -> "weird error"
    in
    failwith
      (Printf.sprintf "line %d, column %d: %s%!" pos.pos_lnum
         (pos.pos_cnum - pos.pos_bol)
         errstr)

let typerror () = raise (TypeError (Printf.sprintf "informative error message"))
let env0 x = raise (UnboundVar x)

let rec eval_expr env = function
  | True -> Bool true
  | False -> Bool false
  | Const n -> Nat n
  | Var x -> env x
  | Not e -> (
      match eval_expr env e with
      | Bool b -> Bool (not b)
      | _ -> typerror ())
  | And (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> typerror ())
  | Or (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> typerror ())
  | Add (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | Nat b1, Nat b2 -> Nat (b1 + b2)
      | _ -> typerror ())
  | Sub (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | Nat b1, Nat b2 -> Nat (b1 - b2)
      | _ -> typerror ())
  | Mul (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | Nat b1, Nat b2 -> Nat (b1 * b2)
      | _ -> typerror ())
  | Eq (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | Nat b1, Nat b2 -> Bool (b1 = b2)
      | _ -> typerror ())
  | Leq (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | Nat b1, Nat b2 -> Bool (b1 <= b2)
      | _ -> typerror ())

exception UnboundVar of string
exception NoRuleApplies

let rec trace1 = function
  | St _ -> raise NoRuleApplies
  | Cmd (cmd, env) -> (
      match cmd with
      | Skip -> St env
      | Assign (x, e) ->
          let v = eval_expr env e in
          let env' y = if y = x then v else env y in
          St env'
      | If (e, c1, c2) -> (
          match eval_expr env e with
          | Bool true -> Cmd (c1, env)
          | Bool false -> Cmd (c2, env)
          | _ -> raise NoRuleApplies)
      | While (e, c) -> (
          match eval_expr env e with
          | Bool true -> Cmd (Seq (c, cmd), env)
          | Bool false -> St env
          | _ -> raise NoRuleApplies)
      | Seq (c1, c2) -> (
          match trace1 (Cmd (c1, env)) with
          | St env' -> Cmd (c2, env')
          | Cmd (c1', env') -> Cmd (Seq (c1', c2), env')))

let trace n cmd =
  let conf0 = Cmd (cmd, env0) in
  let rec helper i conf =
    if i >= n then [ conf ]
    else
      try conf :: helper (i + 1) (trace1 conf) with NoRuleApplies -> [ conf ]
  in
  helper 0 conf0