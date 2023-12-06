open SarithexprLib.Main
open SarithexprLib.Types

type exprtype = BoolT | NatT

exception TypeError of string

let string_of_type = function BoolT -> "Bool" | NatT -> "Nat"

(*Per smettere di ripetere l'errore, sta funzione crea l'errore*)
let typerror e actual expected = raise (
  TypeError (Printf.sprintf "%s has type %s but type %s was expected"
    (string_of_expr e)
    (string_of_type actual)
    (string_of_type expected)
  )
)

let rec typecheck = function
| True | False -> BoolT
| Zero -> NatT
| Not e -> (match typecheck e with
  | BoolT -> BoolT
  (*Creiamo gli errori del typechecker*)
  | _ -> typerror e t BoolT )
(*And e Or richiedono due booleani in ingresso per funzionare*)
| And (e1, e2) | Or (e1, e2)-> ( 
  match typecheck e1, typecheck e2 with
  | BoolT, BoolT -> BoolT
  | t, BoolT -> typerror e1 t BoolT
  | BoolT, t -> typerror e2 t BoolT )
(*Per l'if controlliamo che i valori siano dello stesso tipo*)
| If (e1, e2, e3) -> (match typecheck e1 with
  | BoolT -> 
    let t2, t3 = typecheck e2, typecheck e3 in 
      if t2 = t3 then t2 
      else (raise (TypeError "Type mismatch in if branches"))
  | t -> typerror e t BoolT)

| Succ e | Pred e -> (match e with 
  | NatT -> NatT
  | t -> typerror e t NatT)

| IsZero e -> (match typecheck e with
  | NatT -> BoolT
  | t -> typerror e t NatT)
