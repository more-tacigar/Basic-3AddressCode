rm *.cmi *.cmo parser.ml lexer.ml

ocamlc -c ast.ml
ocamllex lexer.mll
menhir parser.mly --infer
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c main.ml
ocamlc -o main.native lexer.cmo parser.cmo ast.cmo main.cmo
