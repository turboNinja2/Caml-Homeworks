open Plplot
module P = Plot

let n = float_of_string(Sys.argv.(1)) ;;

let rec exp_series x n =
	let rec aux x n i acc coef =
	  if int_of_float(i) == int_of_float(n) then 
		acc 
	  else 
		aux x n (i +. 1.) (acc +. coef) (coef *. x /. i) 
	in 
aux x (n+.1.) 1. 1. x ;;

let f x = exp_series x n ;;

let simple_example g filename n =

  let x_lim = 2.0 in
  
  let p = P.init (~-.x_lim, 0.0) (x_lim, 5.0) `greedy (`svg `core) ~filename:filename in
   
  P.plot ~stream:p [P.func `blue g (~-.x_lim, x_lim) ~step:0.001; P.label "x" "y" ("n = "^string_of_float(n))];
  P.finish ~stream:p ();
  ;;


simple_example f ("exp"^string_of_float(n)^".svg") n; 

