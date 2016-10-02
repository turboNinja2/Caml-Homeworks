type ('state, 'number) state_machine = {
    initial: 'state;
    final: 'state -> string;
    transition: 'number -> 'state -> 'state;
};;

let evaluate machine sequence =
    let rec aux machine state sequence =
        match sequence with 
         [] -> state
        |head::tail -> aux machine ((machine.transition head) state) tail in 
    machine.final (aux machine (machine.initial) sequence)
;;


(*
--->  q0  ---0-->  q2  ---1--->  q1
     /  \         /  \          /  \ 
     -1->         -0 ->        -0,1-> 
*)
let machine_01 = {
    initial = `q0;
    final = (function `q0 -> "Init" | `q2 -> "Found zero" | `q1 -> "END" );
    transition = (function
        | 0 -> (function `q0 -> `q2 | `q2 -> `q2 | `q1 -> `q1)
        | 1 -> (function `q0 -> `q0 | `q2 -> `q1 | `q1 -> `q1)
        | _ -> (function `q0 -> `q0 | `q2 -> `q2 | `q1 -> `q1)
    );
} ;;


let () =
    Printf.printf "------------- start --------------- \n";

    Printf.printf "%s\n" (evaluate machine_01 (1::1::0::0::0::1::0::1::[]));
    Printf.printf "%s\n" (evaluate machine_01 (1::1::0::0::0::[]));
    Printf.printf "%s\n" (evaluate machine_01 (1::1::[]));

    Printf.printf "-------------- end ---------------- \n";
;;
