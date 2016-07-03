open Ocamlbuild_plugin;;

(* Ocaml build doesn't know about llvm_analysis lib, so let it know about it *)
(* use these libraries with use_<lib name> in _tags *)
ocaml_lib ~extern:true "llvm_analysis";;
