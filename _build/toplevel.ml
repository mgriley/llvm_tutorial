let rec main_loop (tokens: Lexer.token list) : unit = 
  begin
  match tokens with
  | Lexer.Ident "ir" :: [] -> Llvm.dump_module Codegen.my_module
  | Lexer.Ident "exit" :: [] -> ()
  | Lexer.Def :: tl ->
      let (func, remaining) = Ast.parse_function tokens in
      print_endline ("defined func: " ^ (Ast.string_of_func func));
      Llvm.dump_value (Codegen.codegen_func func)
  | _ -> 
      let (anon_fun, remaining) = Ast.parse_toplevel tokens in
      print_endline ("anon func: " ^ (Ast.string_of_func anon_fun));
      Llvm.dump_value (Codegen.codegen_func anon_fun)
  end;

  print_string ">> "; flush stdout;
  let tokens : Lexer.token list = Lexer.tokenize_string (read_line ()) in
  main_loop tokens 
   
