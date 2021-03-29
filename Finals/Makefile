default: compile

ML = \
  kanoodle_pieces.ml \
  kanoodle_types.ml \
  kanoodle.ml \
  kanoodle_utils.ml \
  binmat.ml \
  solver.ml

MLI = \
  kanoodle_pieces.mli \
  kanoodle_types.mli \
  kanoodle.mli \
  kanoodle_utils.mli \
  binmat.mli \
  solver.mli

CMO = \
  kanoodle_pieces.cmo \
  kanoodle_types.cmo \
  kanoodle.cmo \
  kanoodle_utils.cmo \
  binmat.cmo \
  solver.cmo

cmi: ${MLI}
	ocamlfind ocamlc -c ${MLI}

cmo: ${ML} cmi
	ocamlfind ocamlc -c ${ML}

compile: main.ml cmo
	ocamlfind ocamlc -c main.ml
	ocamlfind ocamlc ${CMO} main.cmo -o kanoodle

run_tests_binmat:
	ocamlfind ocamlc -c binmat.mli
	ocamlfind ocamlc -c binmat.ml
	ocamlfind ocamlc -o tests_binmat \
	  -package ounit2,str -linkpkg \
	  binmat.cmo tests_binmat.ml
	./tests_binmat

run_tests_solver:
	ocamlfind ocamlc -c binmat.mli
	ocamlfind ocamlc -c binmat.ml
	ocamlfind ocamlc -c solver.mli
	ocamlfind ocamlc -c solver.ml
	ocamlfind ocamlc -o tests_solver \
	  -package ounit2,str -linkpkg \
	  binmat.cmo solver.cmo tests_solver.ml
	./tests_solver

run_tests_kanoodle:
	ocamlfind ocamlc -c kanoodle_pieces.mli
	ocamlfind ocamlc -c kanoodle_pieces.ml
	ocamlfind ocamlc -c kanoodle_types.mli
	ocamlfind ocamlc -c kanoodle_types.ml
	ocamlfind ocamlc -c kanoodle.mli
	ocamlfind ocamlc -c kanoodle.ml
	ocamlfind ocamlc -o tests_kanoodle \
	  -package ounit2,str -linkpkg \
	  kanoodle_pieces.cmo kanoodle_types.cmo kanoodle.cmo tests_kanoodle.ml
	./tests_kanoodle

run_tests: run_tests_binmat run_tests_solver run_tests_kanoodle

clean:
	rm -f *.cmo *.cmi *.cmx *.o kanoodle kanoodle_opt
	rm -f *.log *.cache
	rm -f tests_binmat tests_solver tests_kanoodle
