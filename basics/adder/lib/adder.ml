(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)


(* addList int list -> int *)
let rec addlist l = match l with
| [] -> 0 
| hd :: tl -> hd + addlist tl;;