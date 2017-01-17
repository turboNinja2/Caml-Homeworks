open Complex
open Dense_polynomial_complex

let a = 
  print_string (string_of_complex one);
  print_string "\n";
  print_string (string_of_poly_complex (one::zero::one::[]));
  print_string "\n";
  print_string (string_of_poly_complex (multiply_poly_complex (one::zero::one::[]) (one::one::[])  ));
  print_string "\n";
  print_string (string_of_poly_complex (add_poly_complex (one::zero::one::[]) (one::one::[])  ));
  print_string "\n";
  print_string (string_of_poly_complex (multiply_by_constant_complex (one::zero::one::[]) one  ));
  print_string "\n";
;;

a;
