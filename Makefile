.PHONY: all clear

all:
		ocamlbuild -Is lib,app ftest.native
clear:
		rm -r _build
		rm -f *.cmi *.cmo *.native *.byte
