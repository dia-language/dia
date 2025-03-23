all: lexer
	ocamlc -c main.ml
	# The order is important
	ocamlc diaNode.cmo predefined.cmo lexer.cmo dia.cmo parser.cmo main.cmo -o diac -g

type_predefined:
	ocamlc -c diaNode.ml
	ocamlc -c predefined.ml
	ocamlc -c dia.mli
	ocamlc -c dia.ml

parser: type_predefined
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml

lexer: parser
	ocamllex lexer.mll
	ocamlc -c lexer.ml

clean:
	rm *.cmo *.cmi parser.mli
	rm lexer.ml parser.ml
	rm diac
