open Knn
open Plplot
open Distances
module P = Plot

(* Command line parameters *)
let n_points = int_of_string(Sys.argv.(1)) ;; 
let k = int_of_string(Sys.argv.(2)) ;;
let w = float_of_string(Sys.argv.(3)) ;;

let parameters_string = "Train size = "^string_of_int(n_points)^" - k = "^string_of_int(k)^" - w = "^string_of_float(w) ;;

let filename = "knn_3_"^string_of_int(n_points) ;;

(* Other parameters *)
let n_test_points = 25 ;;
let max_length = 1. ;;

(* Toy data *)
let striped_boundary x y = if (mod_float x  0.3)> 0.1 then 1. else ~-.1. ;;

let unlabelled_boundary x y = 2. ;;

let make_data n_points decision_boundary =
  let output_data = Array.init n_points (fun _ -> (Array.make 2 0.)) in
  let output_label = Array.make n_points 0. in
  for i = 0 to (n_points-1) do
    output_data.(i).(0) <- Random.float max_length;
    output_data.(i).(1) <- Random.float max_length;
    output_label.(i) <- decision_boundary output_data.(i).(0) output_data.(i).(1)
  done;
  output_data, output_label ;;

(* Generates the data *)
let train_data, labels = make_data n_points striped_boundary;;
let test_data, pseudo_labels = make_data n_test_points unlabelled_boundary ;;

let custom_euclide_distance x y = weighted_euclide_distance x y [|1.0;w|] ;;

(* Prediction step for the whole training set *)
let nearest_neighbours = Array.map (fun x -> find_nearest_neighbours x train_data k custom_euclide_distance) test_data;;

let colors_map x = match x with 
  | -1. -> `blue
  | 1. -> `green 
  | _  -> `red ;;

let mismatches = ref 0.;;
let count = ref 0. ;;

(* Ouput the graph in the SVG file *)
let p = P.init (0., 0.) 
  (max_length, max_length) 
  `greedy (`svg `core) 
  ~filename:("knn.svg");; 

P.finish ~stream:p ();

for l = 0 to (n_test_points-1) do
  p = P.init (0., 0.) 
    (max_length, max_length) 
    `greedy (`svg `core) 
    ~filename:(filename^"_"^string_of_int(l)^".svg"); 

  for i = 0 to (n_points-1) do
    P.plot ~stream:p [P.circle ~fill:true (colors_map labels.(i)) train_data.(i).(0) train_data.(i).(1)  0.005];
  done;

  for i = 0 to (n_test_points-1) do
    P.plot ~stream:p [P.circle ~fill:true (colors_map pseudo_labels.(i)) test_data.(i).(0) test_data.(i).(1)  0.005];
  done;
  
  pseudo_labels.(l) <- predict nearest_neighbours.(l) labels ;
 
  if pseudo_labels.(l) <> (striped_boundary test_data.(l).(0) test_data.(l).(1)) then (mismatches := !mismatches +. 1.) else (); 

  for i = 0 to (k-1) do 
    P.plot ~stream:p [P.join `yellow 
      (train_data.(nearest_neighbours.(l).(i)).(0), train_data.(nearest_neighbours.(l).(i)).(1)) 
      (test_data.(l).(0), test_data.(l).(1))
    ];
  done;

  P.plot ~stream:p [P.label "x" "y" 
    (parameters_string^" - Error rate : "^string_of_int(int_of_float(100. *. !mismatches /. float(l+1)))^" % ("^string_of_int(l+1)^")")];

  P.finish ~stream:p ();
done;


