open Dense_polynomial_float
open Plplot
module P = Plot

(* Command line parameters *)
let order = int_of_string(Sys.argv.(1)) ;;

(* Output file name *)
let filename = "chebyshev"^
  "_"^string_of_int(order)^".svg" ;;

let two_x = 0.::2.::[] ;;

(* Recursive definition of Chebyshev polynomials *)
let rec chebyshev_poly n = 
  match n with 
  | 0 -> (1.::[])
  | 1 -> (0.::1.::[])
  | k -> add_poly_float 
	(multiply_by_constant_float (chebyshev_poly (k-2)) ~-.1.) 
	(multiply_poly_float (chebyshev_poly (k-1)) two_x) ;;

let poly = chebyshev_poly order ;;  

let f x = eval_poly_float poly x ;;

(* Plot parameters *)
let x_max = 1. ;;
let y_max = 1. ;;
  
(* Ouput the graph in the SVG file *)
let p = P.init (~-.x_max, ~-.y_max) 
  (x_max, y_max) 
  `greedy (`svg `core) 
  ~filename:filename in

(* Plot the polynomial *)
P.plot ~stream:p [P.func `blue f (~-.x_max, x_max) ~step:0.001;
	P.label "x" "y" (string_of_poly_float poly)];

P.plot ~stream:p [P.func `green (fun x -> 0.) (~-.x_max, x_max) ~step:0.001];

P.finish ~stream:p ();
