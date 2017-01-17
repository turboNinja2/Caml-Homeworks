let get_smallest_elements_i input_array k = 
  let n = Array.length input_array in
  let indices = Array.init n (fun x -> x) in
  for i = 0 to (k-1) do
    for j = (n-1) downto 1 do
      if input_array.(indices.(j-1)) > input_array.(indices.(j)) then begin
        let b = indices.(j-1) in
        indices.(j-1) <- indices.(j);
        indices.(j) <- b;
      end
    done;
  done;
  Array.sub indices 0 k ;;

let find_nearest_neighbours current_point all_points k distance = 
  let distances = Array.map (fun x -> distance x current_point) all_points in
  get_smallest_elements_i distances k ;;

let predict nearest_neighbours labels = 
  let sum a b = a +. b in
  let k = Array.length nearest_neighbours in
  if Array.fold_left sum 0. (Array.init k (fun i -> labels.(nearest_neighbours.(i)))) > 0. then 1. else ~-.1. ;;
