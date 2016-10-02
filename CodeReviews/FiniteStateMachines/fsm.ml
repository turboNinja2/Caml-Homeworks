(*
--->  q0  ---0-->  q2  ---1--->   q1
    /  \         /  \          /    \ 
    -1->         -0->          -0,1-> 
*)

type ('state, 'number) state_machine = {
    initial: 'state;
    final: 'state -> string;
    transition: 'number -> 'state -> 'state;
};;


let state_machine_01 sequence =
    let machine_01 = {
        initial = `q0;
        final = (function `q0 -> "Init" | `q2 -> "Found zero" | `q1 -> "END" );
        transition = (function
            | 0 -> (function `q0 -> `q2 | `q2 -> `q2 | `q1 -> `q1)
            | 1 -> (function `q0 -> `q0 | `q2 -> `q1 | `q1 -> `q1)
            | _ -> (function `q0 -> `q0 | `q2 -> `q2 | `q1 -> `q1)
        );
    } in

    let state = ref machine_01.initial in
    for i = 0 to (List.length sequence) - 1 do
        state := (machine_01.transition (List.nth sequence i)) !state 
    done;
    machine_01.final !state;
;;

let () =
    Printf.printf "------------- start --------------- \n";

    Printf.printf "%s\n" (state_machine_01 [1; 1; 0; 0; 0; 1; 0; 1]);
    Printf.printf "%s\n" (state_machine_01 [1; 1; 0; 0; 0]);
    Printf.printf "%s\n" (state_machine_01 [1; 1; 1]);

    Printf.printf "-------------- end ---------------- \n";
;;
