main.byte: myocamlbuild.ml main.ml ast.ml lexer.ml toplevel.ml codegen.ml ; ocamlbuild -use-ocamlfind -pkg ctypes.foreign main.byte
toplevel.byte : toplevel.ml ast.ml lexer.ml codegen.ml ; ocamlbuild -use-ocamlfind toplevel.byte
ast.byte: ast.ml lexer.ml ; ocamlbuild -use-ocamlfind ast.byte
lexer.byte: lexer.ml ; ocamlbuild -use-ocamlfind lexer.byte 
codegen.byte: codegen.ml ; ocamlbuild -use-ocamlfind codegen.byte
test: test.ml lexer.ml ast.ml ; ocamlfind ocamlc -o test -package oUnit -package Batteries -linkpkg -g lexer.ml ast.ml test.ml
#test.byte: test.ml lexer.ml ast.ml ; ocamlbuild -use-ocamlfind test.byte
