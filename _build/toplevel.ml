open Ctypes;;
(*open Foreign;;*)

let rec main_loop (fpm) (execution_engine) (toplevel_num : int) (tokens: Lexer.token list) : unit = 
  begin
  match tokens with
  | Lexer.Ident "ir" :: [] -> Llvm.dump_module Codegen.my_module
  | Lexer.Def :: tl ->
      let (func, remaining) = Ast.parse_function tokens in
      print_endline ("defined func: " ^ (Ast.string_of_func func));
      Llvm.dump_value (Codegen.codegen_func fpm func)
  | _ -> 
      let suffix = Char.chr toplevel_num in
      let toplevel_name = "toplevel" ^ (Batteries.String.of_char suffix) in
      let (anon_fun, remaining) = Ast.parse_toplevel toplevel_name tokens in
      print_endline ("anon func: " ^ (Ast.string_of_func anon_fun));
      let func : Llvm.llvalue = Codegen.codegen_func fpm anon_fun in
      Llvm.dump_value func;

      (*TODO: use dir +foreign *)
      (* JIT the function, which gives a function pointer *)

      (* create a Ctypes.typ that is a pointer to a function of type () -> float *)
      let fun_type : (unit -> float) Ctypes.fn = Ctypes.void @-> returning Ctypes.double in
      let funptr_type = Foreign.funptr fun_type in (* conver the func to a func ptr *)

      (* get the function address of our anonymous function, then execute the function *)
      (* llvm calls a function will name "" @0, then #s upwards from there *)
      let ocaml_funptr = Llvm_executionengine.get_function_address toplevel_name funptr_type execution_engine in 
      let result = ocaml_funptr () in (* call the function *)
      print_float result; 
      print_newline ();

      (* delete this function, it will not be used again *)
      Llvm.delete_function func;
  end;

  print_string ">> "; flush stdout;
  let tokens : Lexer.token list = Lexer.tokenize_string (read_line ()) in
  main_loop fpm execution_engine (toplevel_num + 1) tokens 
   
