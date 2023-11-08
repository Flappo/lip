open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl


(* Costruire il dizionario di token*) 
(* la fold_lef applica il primo ingresso (funzione) al secondo elemento basandosi sul terzo*)
let rec inc (freqdic : (token * int) list) (t : token) : (token * int) list =
  match freqdic with
  | [] -> [(t,1)]
  | (t',n)::dic' when t'=t -> (t, n+1)::dic' (* match il nostro dizionario è una coppia il cui token è uguale a quello passato come argomento*)
  | x::dic' -> x::(inc dic' t) (* match qualcosa che non è il nostro token corrente *)

(* Produce il dizionario che cercavamo *)    (*questo qua sotto si può riscrivere come "inc"*)
let rec frequency1 tokenlist = List.fold_left (fun acc t -> inc acc t) [] tokenlist 

(* prende i primi n elementi della lista *)
let rec take n = function
| [] -> []
| _ when n=0 -> []
| x::t -> x::(take (n-1) t)

(* Ci restituisce i primi n elementi di una lista in ordine di quante volte appare quel token(con rispettivo numero di volte)
   frequency : int -> 'a list -> ('a * int) list *)                                                
let frequency n tokenlist = (* Ordiniamo la lista in base alle frequenze*)
    frequency1 tokenlist |> List.sort (fun (_,n1) (_,n2) -> compare n2 n1) (* compare al contrario perchè la lista è al contrario *)

