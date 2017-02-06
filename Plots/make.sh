ocamlfind opt -package plplot -linkpkg -o fourier_fejer FourierFejer.ml
ocamlfind opt -package plplot -linkpkg -o exponential Exponential.ml
ocamlfind opt -package plplot -linkpkg -o nyquist Nyquist.ml

ocamlopt dense_polynomial.ml dense_polynomial_complex.ml test_complex_poly.ml -o test_complex_poly 

ocamlfind opt -package plplot -linkpkg -o 3dpoly dense_polynomial.ml dense_polynomial_complex.ml plot_complex_polynomial.ml
ocamlfind opt -package plplot -linkpkg -o 3d_chebyshev dense_polynomial.ml dense_polynomial_complex.ml plot_complex_chebyshev.ml
ocamlfind opt -package plplot -linkpkg -o poly_plot dense_polynomial.ml dense_polynomial_float.ml plot_poly.ml
ocamlfind opt -package plplot -linkpkg -o chebyshev_plot dense_polynomial.ml dense_polynomial_float.ml chebyshev_poly.ml
ocamlfind opt -package plplot -linkpkg -o knn_plot knn.ml knn_in_action.ml




rm *.cmi 
rm *.cmx
rm *.o
