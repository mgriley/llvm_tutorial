open Ctypes
open Llvm_scalar_opts
(*open Foreign;;*)

(* TODO read the tutorial at top of README, it addresses the error. Problem is that we compile the module, then try to add new stuff to it. Need to use more modules instead *)

let rec main_loop (tokens: Lexer.token list) : unit = 
  begin
  match tokens with
  | Lexer.Ident "ir" :: [] -> Llvm.dump_module Codegen.my_module
  | Lexer.Def :: tl ->
      let (func, remaining) = Ast.parse_function tokens in
      print_endline ("defined func: " ^ (Ast.string_of_func func));
      Llvm.dump_value (Codegen.codegen_func func)
  | Lexer.Extern :: tl ->
      let (proto, remaining) = Ast.parse_extern tokens in
      print_endline  ("declared extern: " ^ (Ast.string_of_extern proto));
      Llvm.dump_value (Codegen.codegen_proto proto)
  | _ -> 
      let toplevel_name = "toplevel" in
      let (anon_fun, remaining) = Ast.parse_toplevel toplevel_name tokens in
      print_endline ("anon func: " ^ (Ast.string_of_func anon_fun));
      let func : Llvm.llvalue = Codegen.codegen_func anon_fun in
      Llvm.dump_value func;

      (* JIT the function, which gives a function pointer *)

      (* create an execution engine for the module *)
      (* NB: creating it will create a new execution engine (forget the compiled code from old one) *)
      (* which is what we require to parse new top-level expressions without referring to old ones *)
      let execution_engine = Llvm_executionengine.create Codegen.my_module in
      
      (* create optimizer pipeline *)

      (* create function pass manager that operates on the whole module, rather than function by function *)
      let fpm = Llvm.PassManager.create () in

      (* specifies how target lays out data structures *)
      Llvm_target.DataLayout.add_to_pass_manager fpm (Llvm_executionengine.data_layout execution_engine);

      (* add passes (from llvm_scalar_opts) *)
      add_instruction_combination fpm; (* peephole and bit twiddling *)
      add_reassociation fpm; (* reassociate expressions *)
      add_gvn fpm; (* eliminate common subexpressions *)
      add_cfg_simplification fpm; (* simplify control flow graph (ex. remove unreachable blocks) *)

      (* initialize, execute, and finalize function passes on the module *)
      let did_modify = Llvm.PassManager.run_module Codegen.my_module fpm in
      if did_modify then () else print_endline "FPM did not modify module";

      (* get the function pointer and execute *)

      (* create a Ctypes.typ that is a pointer to a function of type () -> float *)
      let fun_type : (unit -> float) Ctypes.fn = Ctypes.void @-> returning Ctypes.double in
      let funptr_type = Foreign.funptr fun_type in (* conver the func to a func ptr *)

      (* get the function address of our anonymous function, then execute the function *)
      (* llvm calls a function will name "" @0, then #s upwards from there *)
      let ocaml_funptr = Llvm_executionengine.get_function_address toplevel_name funptr_type execution_engine in 
      let result = ocaml_funptr () in (* call the function *)
      Printf.printf "%.3f" result; 
      print_newline ();

      (* delete this function from the module, it will not be used again *)
      Llvm.delete_function func;
  end;

  print_string ">> "; flush stdout;
  let tokens : Lexer.token list = Lexer.tokenize_string (read_line ()) in
  main_loop tokens 
   
