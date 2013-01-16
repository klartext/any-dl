
#all: native-code
all: byte-code
#all: debug-code


LIBS=unix pcre netstring netsys netclient curl xml-light

SOURCES :=  parsetreetypes.ml scriptparser.mly scriptlexer.mll parsers.ml network.ml cli.ml main.ml
#SOURCES :=  parsers.ml network.ml main.ml 

PACKS := pcre netstring netsys netclient curl xml-light

YFLAGS=-v
#OCAMLYACC = menhir
OCAMLYACC = ocamlyacc

RESULT = any-dl


include OCamlMakefile
