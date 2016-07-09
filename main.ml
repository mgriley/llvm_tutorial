open Llvm
open Llvm_scalar_opts

let () = 

  Printexc.record_backtrace true; (* for debugging via stacktrace *)

  (* create the JIT *)  

  (*  NB: must initialize the execution engine before use *)
  let result = Llvm_executionengine.initialize() in 
  if result then () else failwith "Could not init Exec engine";
  let execution_engine = Llvm_executionengine.create Codegen.my_module in
  let fpm = PassManager.create_function Codegen.my_module in
  
  (* Setup optimizer pipline *)

  (* specifies how target lays out data structures *)
  Llvm_target.DataLayout.add_to_pass_manager fpm (Llvm_executionengine.data_layout execution_engine);

  (* add passes (from llvm_scalar_opts *)
  add_instruction_combination fpm; (* peephole and bit twiddling *)
  add_reassociation fpm; (* reassociate expressions *)
  add_gvn fpm; (* eliminate common subexpressions *)
  add_cfg_simplification fpm; (* simplify control flow graph (ex. remove unreachable blocks) *)

  let _ = Llvm.PassManager.initialize fpm in 

  (* run interpreter loop *)
  try
    print_string ">> "; flush stdout;
    let next_line = read_line () in
    let first_tokens = Lexer.tokenize_string next_line in
    Toplevel.main_loop fpm execution_engine 97 first_tokens
  with e -> 
    print_endline (Printexc.to_string e);
    Printexc.print_backtrace stdout


  
 
