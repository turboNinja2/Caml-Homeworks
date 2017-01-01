ocamlfind opt -package plplot -linkpkg -o fourier_fejer FourierFejer.ml
ocamlfind opt -package plplot -linkpkg -o exponential Exponential.ml

rm *.cmi 
rm *.cmx
rm *.o
