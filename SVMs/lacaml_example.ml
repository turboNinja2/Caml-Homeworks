(* prints matrix m (with header name) on formatter out_f *)
let print_matrix out_f name m =
  Format.fprintf out_f "@[<2>%s :@\n@\n@[%a@]@]@\n@\n" name Lacaml.Io.pp_fmat m

(* idem with vectors... *)
let print_vect out_f name v =
  Format.fprintf out_f "@[<2>%s :@\n@\n@[%a@]@]@\n@\n" name Lacaml.Io.pp_fvec v

(* we need the sum of the entries of v, not its 1-norm (given by asum) *)
let sum_of_vect x =
  let n = Lacaml.D.Vec.dim x and s = ref 0. in
  for k = 1 to n do
    s := !s +. x.{k}
  done;
  !s

let dominant_left_eigen_vect m =
  let aux = Lacaml.D.lacpy m in
  let (left_eig_vec, eig_val, _, _) = Lacaml.D.geev aux in
  let v = Lacaml.D.Mat.col left_eig_vec (Lacaml.D.iamax eig_val) in
  Lacaml.D.scal (1. /. sum_of_vect v) v;
  v

(* quick'n dirty way to get a random primitive matrix with probability 1 *)
let random_primitive n =
let m = Lacaml.D.Mat.random n n in
for i = 1 to n do
  for j = 1 to n do
    m.{i, j} <- (abs_float m.{i, j})
  done
done;
m

let main () =
  let n = int_of_string Sys.argv.(1) in
  let out_f = Format.formatter_of_out_channel stdout
  and m = random_primitive n in
  print_matrix out_f "Random primitive matrix" m;
  let v = dominant_left_eigen_vect m in
  print_vect out_f "Dominant left eigen vector" v
;;

main ();;
