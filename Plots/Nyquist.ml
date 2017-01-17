open Plplot
module P = Plot

let sampling_frequency = float_of_string(Sys.argv.(1)) ;;
let alpha = float_of_string(Sys.argv.(2)) ;;

let pi = 3.1415926535 ;;

let signal x = cos (2.0*.pi*.(sampling_frequency/.2.+.alpha)*.x) ;;

let alias_signal x = cos (2.0*.pi*.(sampling_frequency/.2.-.alpha)*.x) ;;

let simple_example f f_alias filename n =

  let x_max = 1.0 in
  let y_max = 1.5 in
  
  let p = P.init (0.0, ~-.y_max) (x_max, y_max) `greedy (`svg `core) ~filename:filename in
  
  P.plot ~stream:p [P.func `blue f (0.0, x_max) ~step:0.001;
	P.label "x" "y" ("Sampling Frequency = "^string_of_float(n)^"\t alpha = "^string_of_float(alpha))];
   
  P.plot ~stream:p [P.func `green f_alias (0.0, x_max) ~step:0.001];
  P.finish ~stream:p ();
  ;;

simple_example signal alias_signal ("nyquist"^string_of_float(sampling_frequency)^string_of_float(alpha)^".svg") sampling_frequency ;;

