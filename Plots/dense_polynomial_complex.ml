open Complex
open Dense_polynomial

let string_of_complex z = 
  if z.im <> 0. then 
    "("^string_of_float(z.re)^"+ "^string_of_float(z.im)^"i)" 
  else
    string_of_float(z.re) ;;

let complex_of_int n = {re = float_of_int(n); im = 0.} ;;

let mul_int_float a b = mul (complex_of_int a) b ;;

let add_poly_complex p1 p2 = add_poly p1 p2 add ;;

let multiply_by_identity_complex p = multiply_by_identity p zero ;;

let multiply_by_constant_complex p a = multiply_by_constant p a mul;;

let string_of_poly_complex p = string_of_poly p string_of_complex zero one;;

let multiply_poly_complex p1 p2 = multiply_poly p1 p2 add mul zero ;;

let power_complex p n = power p n add mul zero ;;  

let eval_poly_complex p x = eval_poly p x add mul zero ;;

let derivative_complex p = derivative p mul_int_float ;;

let n_derivative_complex p order = n_derivative p order mul_int_float ;;
