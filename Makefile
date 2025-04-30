OCAMLCOMP=ocamlopt
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

all: ocamlopt

ocamlrun: OCAMLCOMP=ocamlc
ocamlrun: lexer
	$(OCAMLCOMP) -c main.ml
	# The order is important
	$(OCAMLCOMP) diaNode.cmo predefined.cmo lexer.cmo dia.cmo parser.cmo main.cmo -o diac -g

ocamlopt: lexer
	$(OCAMLCOMP) -c main.ml
	${OCAMLCOMP} diaNode.cmx predefined.cmx lexer.cmx dia.cmx parser.cmx main.cmx -o diac -g

type_predefined:
	$(OCAMLCOMP) -c diaNode.ml
	$(OCAMLCOMP) -c predefined.ml
	$(OCAMLCOMP) -c dia.mli
	$(OCAMLCOMP) -c dia.ml

parser: type_predefined
	$(OCAMLYACC) parser.mly
	$(OCAMLCOMP) -c parser.mli
	$(OCAMLCOMP) -c parser.ml

lexer: parser
	$(OCAMLLEX) lexer.mll
	$(OCAMLCOMP) -c lexer.ml

clean:
	rm -f *.o *.cmo *.cmx *.cmi parser.mli
	rm -f lexer.ml parser.ml
	rm -f diac
