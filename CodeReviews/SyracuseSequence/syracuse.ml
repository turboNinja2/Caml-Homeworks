#load "unix.cma";;
open Unix;;

let time f x =
    let start = Unix.gettimeofday ()
    in let res = f x
    in let stop = Unix.gettimeofday ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
    in res ;;


let syracuse1 x =
    let rec aux x t = 
        match x with
          1 -> t
          |x -> if x mod 2 = 0 then aux (x / 2) (t + 1) else aux (3 * x + 1) (t+1) in
     aux x 0 ;; 

let syracuse2 x =
    let rec aux x t = 
        if x = 1 then 
            t
        else 
            if x mod 2 = 0 then aux (x / 2) (t + 1) else aux (3 * x + 1) (t+1)
        in
     aux x 0 ;; 

let syracuse3 x =
    let rec aux x t = 
         match x with 
          1 -> t
         |2 -> t + 1
         |4 -> t + 2
         |s-> match s mod 4 with 
               0 -> aux (x/4) (t+2)
               |2 -> aux (x/2) (t+1)
               |_ -> aux (3*x+1) (t+1) in

     aux x 0 ;; 


let rec iterate_over n f l  = 
     if n = 0 then l else iterate_over (n-1) f ((f n)::l);;

let syracuses1 n = iterate_over n syracuse1 [];;
let syracuses2 n = iterate_over n syracuse2 [];;
let syracuses3 n = iterate_over n syracuse3 [];;

assert(syracuse1 12 = syracuse2 12);;
assert(syracuse1 12 = syracuse3 12);;

(time syracuses1 100000);;
(time syracuses2 100000);;
(time syracuses3 100000);;

