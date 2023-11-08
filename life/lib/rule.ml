(*
* Define a type rule to represent rules in the S/B form, and make the alive 
* function parameteric on a S/B rule. Its type must be:
* alive : bool list list -> int -> int -> Life.Rule.rule -> bool   
*)

type rule = int list * int list