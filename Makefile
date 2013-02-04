
#all: native-code
all: byte-code
#all: debug-code


SOURCES :=  cli.ml parsetreetypes.ml scriptparser.mly scriptlexer.mll parsers.ml network.ml main.ml

PACKS := pcre netstring netsys netclient curl xml-light

YFLAGS=-v
#OCAMLYACC = menhir
OCAMLYACC = ocamlyacc

RESULT = any-dl


include OCamlMakefile
