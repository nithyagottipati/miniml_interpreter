all: miniml expr evaluation expr_test evaluation_test 

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr_test: expr_test.ml
	ocamlbuild -use-ocamlfind expr_test.byte 

evaluation_test: evaluation_test.ml
	ocamlbuild -use-ocamlfind evaluation_test.byte 


clean:
	rm -rf _build *.byte
