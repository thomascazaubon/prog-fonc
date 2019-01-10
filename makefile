.PHONY: all clear img ftest

#HOW TO USE : make f=[my_graph] s=[source] d=[destination]
#
#[] <=> to be precised without the []

#Default values, used if none are precised explicitely
f ?= graphmulti
s ?= 2
d ?= 2

all: build ftest export

#Just builds the files
build:
	ocamlbuild -Is lib,app multisource.native

#Executes the ftest file
ftest: build
	./multisource.native graph/$(f) $(s) $(d) FF_graph 0 1 4 5

#Exports the output of ftest into an image
export: build ftest
	dot -Tpng FF_export > FF_img

#Clears all the files created by make
clear:
		rm -r _build
		rm -f *.cmi *.cmo *.native *.byte FF_export FF_img FF_graph
