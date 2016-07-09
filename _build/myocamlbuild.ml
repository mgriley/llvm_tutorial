open Ocamlbuild_plugin;;

(* Ocaml build doesn't know about llvm_analysis lib, so let it know about it *)
(* use these libraries with use_<lib name> in _tags *)
ocaml_lib ~extern:true "llvm_analysis";;
ocaml_lib ~extern:true "llvm_executionengine";;
ocaml_lib ~extern:true "llvm_target";;
ocaml_lib ~extern:true "llvm_scalar_opts";;
(*ocaml_lib ~extern:true ~dir:"/Users/matthewriley/.opam/4.02.3/lib/ctypes/" "foreign";;*)
(*ocaml_lib ~extern:true "ctypes/ctypes_static";; (* from ctypes *)*)

(*flag ["link"; "ocaml"; "g++"] (S[A"-cc"; A"g++"]);;*)

(* file with tags link, ocaml, use_bindings depends on 'bindings.o' *)
(*dep ["link"; "ocaml"; "use_bindings"] ["bindings.o"];;*)
