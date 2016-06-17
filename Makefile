main.byte: main.ml ast.ml lexer.ml ; ocamlbuild -use-ocamlfind main.byte
ast.byte: ast.ml lexer.ml ; ocamlbuild -use-ocamlfind ast.byte
lexer.byte: lexer.ml ; ocamlbuild -use-ocamlfind lexer.byte 
test: test.ml lexer.ml ; ocamlfind ocamlc -o test -package oUnit -package Batteries -linkpkg -g lexer.ml test.ml
