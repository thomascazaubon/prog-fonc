.PHONY: all clear img ftest

all:
	ocamlbuild -Is lib,app ftest.native
	./ftest.native graph/graph1 0 3 testgraph
	dot -Tpng testexport > testimg

default:
	ocamlbuild -Is lib,app ftest.native

clear:
		rm -r _build
		rm -f *.cmi *.cmo *.native *.byte testexport testimg testgraph

ftest:
	./ftest.native graph/graph1 0 3 testgraph

img:
	dot -Tpng testexport > testimg
