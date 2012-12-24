
all: byte-code
#all: debug-code


LIBS=unix pcre netstring netsys netclient curl xml-light

SOURCES :=  scriptparser.mly scriptlexer.mll parsers.ml network.ml parsetreetypes.ml main.ml
#SOURCES :=  parsers.ml network.ml main.ml 

PACKS := pcre netstring netsys netclient curl xml-light

YFLAGS=-v
#OCAMLYACC = menhir
OCAMLYACC = ocamlyacc

RESULT = any-dl



include OCamlMakefile
