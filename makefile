.PHONY: all clear img ftest

all:
	ocamlbuild -Is lib,app ftest.native

clear:
		rm -r _build
		rm -f *.cmi *.cmo *.native *.byte testexport testimg testgraph

ftest:
	./ftest.native graph/graph2.txt 0 8 testgraph

img:
	dot -Tpng testexport > testimg

