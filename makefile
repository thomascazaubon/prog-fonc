.PHONY: all clear build ftest export random

#HOW TO USE : make f=[my_graph] s=[source] d=[destination]
#[] <=> to be precised without the []

#Default values, used if none are precised explicitely
f ?= random
s ?= 0
d ?= 9

all: build ftest export

#Just builds the files
build:
	ocamlbuild -Is lib,app ftest.native

#Executes the ftest file
ftest: build
	./ftest.native graph/$(f) $(s) $(d) FF_graph

#Exports the output of ftest into an image
export: build ftest
	dot -Tpng FF_export > FF_img

random:
		ocamlbuild -Is lib,app random_generator.native
		./random_generator.native graph/random $(s)

#Clears all the files created by make
clear:
		rm -r _build
		rm -f *.cmi *.cmo *.native *.byte FF_export FF_img FF_graph
