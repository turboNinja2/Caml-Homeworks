let add_poly p1 p2 addition =
  let rec aux p1 p2 acc =
    match p1, p2 with 
    | [], [] -> acc
    | [], h2::t2 -> aux [] t2 (h2::acc) 
    | h1::t1, [] -> aux t1 [] (h1::acc)
    | h1::t1, h2::t2 -> aux t1 t2 ((addition h1 h2)::acc)
  in List.rev (aux p1 p2 []);;

let multiply_by_identity p zero = zero::p ;;

let rec multiply_by_constant p a multiplication =
  match p with 
  | [] -> []
  | h::t -> (multiplication a h)::(multiply_by_constant t a multiplication) ;;

let multiply_poly p1 p2 addition multiplication zero = 
  let rec aux p1 p2 acc =
    match p1 with
    | [] -> acc
    | h::t -> aux t (multiply_by_identity p2 zero) (add_poly acc (multiply_by_constant p2 h multiplication) addition)
  in aux p1 p2 [];;

let power p n addition multiplication zero = 
  let rec aux p n = match n with
    | 1 -> p
    | k -> multiply_poly p (aux p (k-1)) addition multiplication zero
  in aux p n ;;

let derivative p multiply_by_int =
  let rec aux p n = match p with 
    | [] -> []
    | h::t ->  (multiply_by_int n h)::(aux t (n+1)) in 
  List.tl (aux p 0) ;;

let rec n_derivative p order multiply_by_int = 
  match order with 
    | 0 -> p
    | k -> n_derivative (derivative p multiply_by_int) (order - 1) multiply_by_int;;

let eval_poly p x addition multiplication zero = 
  let rec aux p acc = match p with
    | [] -> acc
    | h::t -> aux t (addition h (multiplication x acc)) 
  in aux (List.rev p) zero ;;

let string_of_poly poly string_of_coef zero neutral = 
  let rec aux p i acc = 
    match p with 
    | [] -> acc
    | h::t -> if h <> zero then 
              begin 
                if h <> neutral then
                  aux t (i+1) (string_of_coef(h)^"x^"^string_of_int(i)^" + "^acc)
                else
                  aux t (i+1) ("x^"^string_of_int(i)^" + "^acc)
              end 
              else aux t (i+1) acc
    in aux (List.tl poly) 1 (string_of_coef (List.hd poly)) ;;
