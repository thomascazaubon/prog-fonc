.PHONY: all clear build ftest export random

#######   SIMPLE USE -> CREATES A RANDOM GRAPH GIVEN A NUMBER OF NODES AND EXECUTES FORD FULKERSON ON IT  ######
	#### BE CAREFUL, 20 NODES SEEM TO BE A MAXIMUM ! ####
	#make run s=[number_of_nodes]

####### ADVANCED USE #######
	#RUNNING THE BASIC FF ALGORITHM
	#make f=[my_graph] s=[source] d=[destination]
	#RUNNING THE MULTI-SOURCE MULTI-SINK FF ALGORITHM
	#!!! YOU NEED TO MANUALLY SET THE LIST OF SOURCE NODES FOLLOWED BY THE LIST OF SINK NODES IN THE "MULTI" TAG !!!
	#make MULTI f=[my_graph] s=[number_of_sources] d=[number_of_sinks]
	#GENERATING A RANDOM GRAPH
	#make random s=[number_of_nodes]

#[] <=> to be precised without the []

#Default values, used if none are precised explicitely
f ?= random
s ?= SRC
d ?= DST

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

#Generates a random graph
random:
		ocamlbuild -Is lib,app random_generator.native
		./random_generator.native graph/random $(s)

#Executes a multi-source multi-sinks FF algorithm
multi:
	ocamlbuild -Is lib,app multisource.native
	./multisource.native graph/$(f) $(s) $(d) FF_graph 1 2 4 5

#Clears all the files created by make
clear:
		rm -r _build
		rm -f *.cmi *.cmo *.native *.byte FF_export FF_img FF_graph

#Quick run on a normal graph
run : random build
		./ftest.native graph/$(f) SRC DST FF_graph
		dot -Tpng FF_export > FF_img
