let euclide_distance x y =
  let sum a b = a +. b in
  let squares_diff = Array.init (Array.length x) (fun i -> (x.(i) -. y.(i))**2.) in
  Array.fold_left sum 0. squares_diff ;;

let weighted_euclide_distance x y w =
  let sum a b = a +. b in
  let squares_diff = Array.init (Array.length x) (fun i -> (w.(i) *. (x.(i) -. y.(i)))**2.) in
  Array.fold_left sum 0. squares_diff ;;

