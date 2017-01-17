open Dense_polynomial

let add_float a b = a +. b ;;

let mul_float a b = a *. b ;;

let mul_int_float a b = float_of_int(a) *. b ;;

let add_poly_float p1 p2 = add_poly p1 p2 add_float ;;

let multiply_by_identity_float p = multiply_by_identity p 0. ;;

let multiply_by_constant_float p a = multiply_by_constant p a mul_float;;

let string_of_poly_float p = string_of_poly p string_of_float 0. 1.;;

let multiply_poly_float p1 p2 = multiply_poly p1 p2 add_float mul_float 0. ;;

let power_float p n = power p n add_float mul_float 0.;;  

let eval_poly_float p x = eval_poly p x add_float mul_float 0. ;;

let derivative_float p = derivative p mul_int_float ;;

let n_derivative_float p order = n_derivative p order mul_int_float ;;

