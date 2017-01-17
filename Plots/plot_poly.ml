open Dense_polynomial_float
open Plplot
module P = Plot

(* Command line parameters *)
let degree = int_of_string(Sys.argv.(1)) 

let order = int_of_string(Sys.argv.(2))

(* Output file name *)
let filename = "poly"^string_of_int(degree)^
  "_"^string_of_int(order)^".svg" ;;

(* P : X -> (X^2 - 1)^degree (order) *)
let poly = n_derivative_float (power_float ((~-.1.0)::0.0::1.0::[]) degree) order;;

let f x = eval_poly_float poly x ;;

(* Plot parameters *)
let x_max = 1.5 ;;
let y_max = 6. ;;
  
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
