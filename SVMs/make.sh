ocamlfind ocamlopt -thread -o svm_prog -linkpkg -package lacaml -package libsvm svm_example.ml
ocamlfind ocamlopt -o lacaml_prog -linkpkg -package lacaml lacaml_example.ml

rm *.cmi 
rm *.cmx
rm *.o

