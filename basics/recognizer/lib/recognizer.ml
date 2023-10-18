(* let reg1 = Str.regexp "[01]+";; *)
let lang1 l = match l with
| [] -> false
| '0'::_ | '1'::_ -> true
| _ -> false


(* let reg2 = Str.regexp "0?1*";; *)
let  lang2 l = match l with
| '0'::'1'::tl | '1'::tl -> let rec leng2AUX tl = match tl with
                            | '1'::[] -> true
                            | '1'::tl -> leng2AUX tl
                            | '0'::_ -> false
                            | [] | _ -> false
in leng2AUX tl
| _ -> false



(*let reg3 = Str.regexp "0[01]*0";; *)
let lang3 l = match l with
| '1'::[] | '0'::[] | [] -> false
| '0'::tl -> let rec leng3AUX tl = match tl with 
             | '0'::[] -> true
             | '1'::[] | [] -> false
             | _::tl -> leng3AUX tl
in leng3AUX tl
| _::_ -> false


(*let reg4 = Str.regexp "0*10*10*";;*)
let lang4 l = match l with
| '0'::[] | '1'::'0'::[] | [] -> true
| '0'::tl | '1'::'0'::tl -> let rec leng4AUX tl = match tl with
                     | [] | '1'::'0'::[] -> true
                     | '1'::'1'::_ | '0'::'0'::_ -> false
                     | '1'::'0'::tl -> leng4AUX tl
                     | _::_ -> false
in leng4AUX tl 
| _::_ -> false

(*let reg5 = Str.regexp "(00|11)+";;*)
let lang5 l = match l with
| '0'::'0'::[] | '1'::'1'::[] -> true
| '0'::'0'::tl -> let rec leng5AUX0 tl = match tl with
                  | '1'::_ | [] -> false
                  | '0'::'0'::[] -> true
                  | '0'::'0'::tl -> leng5AUX0 tl
                  | _::_ -> false
in leng5AUX0 tl
| '1'::'1'::tl -> let rec leng5AUX1 tl = match tl with
                  | '0'::_ | [] -> false
                  | '1'::'1'::[] -> true
                  | '1'::'1'::tl -> leng5AUX1 tl
                  | _::_ -> false
in leng5AUX1 tl
| '0'::'1'::_ | '1'::'0'::_ | _::_ | [] -> false
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
