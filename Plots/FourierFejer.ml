open Plplot
module P = Plot

let n = int_of_string(Sys.argv.(1)) ;;


let cesaro_series x n f =
  let rec partial_sums x n f sum = 
    if n = 1 then sum else partial_sums x (n-1) f (sum +. (f x n))   
  in (partial_sums x n f 0.) /. float_of_int(n+1) ;;


let rec fourier_series_sawtooth x n =
  let float_n = float_of_int(n) in
  if n = 0 then 0. else (sin(float_n *. x)) /. float_n +. fourier_series_sawtooth x (n-1) ;;


let f x = fourier_series_sawtooth x n ;;


let g x = cesaro_series x n fourier_series_sawtooth ;;


let simple_example g filename n =
  
  let p = P.init (0.0, -2.0) (10.0, 2.0) `greedy (`svg `core) ~filename:filename in
   
  P.plot ~stream:p [P.func `blue g (0.0, 10.0) ~step:0.001; P.label "x" "y" ("n = "^string_of_int(n))];
  P.finish ~stream:p ();
  ;;


simple_example f ("fourier"^string_of_int(n)^".svg") n; 
simple_example g ("fejer"^string_of_int(n)^".svg") n;
