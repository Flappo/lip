module T = ANSITerminal
open Printf

let parse (s : string) : Rule.rule = 
  let lexbuf = Lexer.from_string s in 
  let r = Parser.rule Lexer.read_token lexbuf in r

(* let rec range a b = if b<a then [] else a::(range (a+1) b) *)

let rec zeroes = function
    0 -> []
  | n -> false::(zeroes (n-1))

let string_of_world1 w1 =
  List.fold_left (fun s x -> s ^ (if x then "*" else " ")) "" w1

let string_of_world w =
  List.fold_left (fun s x -> s ^ "\n" ^ string_of_world1 x) "" w

(* p in [0,100] is the probability of 1 *)

let rec rnd_world1 p = function
    0 -> []
  | n -> (Random.int(100)<p)::(rnd_world1 p (n-1))

let rec rnd_world p m = function
  0 -> []
| n -> (rnd_world1 p m) :: (rnd_world p m (n-1))

let init_w = rnd_world 20 30 60

let neighbours w i j = 
  let n = List.length w in
  let l0 = List.nth w ((i+n-1) mod n) 
  and l1 = List.nth w (i mod n) 
  and l2 = List.nth w ((i+1) mod n) 
  in 
  let m = List.length l0
  in  
  assert (List.length l1 = m && List.length l2 = m);
  (
    List.nth l1 (j mod m),
    [
      [List.nth l0 ((j+m-1) mod m); List.nth l0 (j mod m); List.nth l0 ((j+1) mod m)]; 
      [List.nth l1 ((j+m-1) mod m); false; List.nth l1 ((j+1) mod m)]; 
      [List.nth l2 ((j+m-1) mod m); List.nth l2 (j mod m); List.nth l2 ((j+1) mod m)]
    ])

let count1 l = List.fold_left (fun s x -> s + (if x then 1 else 0)) 0 l

let count w = List.fold_left (fun s x -> s + count1 x) 0 w

(*
Its type must be:
alive : bool list list -> int -> int -> Life.Rule.rule -> bool
*)

(* alive: bool list list -> int -> int -> bool *)
let alive w i j ((l1, l2) : Rule.rule) =
  let (cell,nb) = neighbours w i j in
  let alive_nb = count nb in
  if cell then (* cell is alive *)
    (* cell survives? *)
    List.fold_left (fun acc n -> n = alive_nb || acc) false l1
  else (* cell is dead *)
    (* cell is born? *)
    List.fold_left (fun acc n -> n = alive_nb || acc) false l2

(*
  let alive w i j ((l1, l2) : Rule.rule)=
  let (cell,nb) = neighbours w i j in
  let alive_nb = count nb in
  if cell then (* cell is alive *)
    (* cell survives? *)
    alive_nb = 2 || alive_nb = 3
  else (* cell is dead *)
    (* cell is born? *)
    alive_nb = 3
*)


let step1 w i r =
  let n = List.length w in
  List.mapi (fun j _ -> alive w i j r) (zeroes n)

let step w r =
  let n = List.length w in
  List.mapi (fun i _ -> step1 w i r) (zeroes n)

(* let step w = List.map step1 w *)
(* let step w = w *)

let display w =
  T.erase T.Screen;
  T.set_cursor 1 1;
  (* T.print_string [] (string_of_world w); *)
  printf "%s\n%!" (string_of_world w);
  Unix.sleepf 0.15;;

let rec loop w n r = (* modifichiamo loop per r*)
  if n=0 then (display w; w)
  else (display w; loop (step w r) (n-1))
