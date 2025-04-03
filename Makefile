OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

all: lexer
	$(OCAMLC) -c main.ml
	# The order is important
	$(OCAMLC) diaNode.cmo predefined.cmo lexer.cmo dia.cmo parser.cmo main.cmo -o diac -g

type_predefined:
	$(OCAMLC) -c diaNode.ml
	$(OCAMLC) -c predefined.ml
	$(OCAMLC) -c dia.mli
	$(OCAMLC) -c dia.ml

parser: type_predefined
	$(OCAMLYACC) parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c parser.ml

lexer: parser
	$(OCAMLLEX) lexer.mll
	$(OCAMLC) -c lexer.ml

clean:
	rm *.cmo *.cmi parser.mli
	rm lexer.ml parser.ml
	rm diac
