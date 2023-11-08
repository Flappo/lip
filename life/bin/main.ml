module T = ANSITerminal
open Life.Main


(* Questa funzione legge il comando dalla console e tratta la stringa in input come un array *)
let _ = match Array.length(Sys.argv) with (* in argv il primo elemento è il comando Life e il secondo il numero che gli diamo *)
    2 -> let k = int_of_string (Sys.argv.(2)) in (* Ora che abbiamo argv.(2) abbiamo 3 ingressi, Life, Regola e numero *)
    let rule = parse (Sys.argv.(1)) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let w = loop init_w k rule in (* loop prende la regola *)
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()
  (* wrong usage *)
  | _ -> failwith "Usage: dune exec life n_rounds"
