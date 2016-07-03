(*let () =*)
    (*let file: in_channel = open_in "sample.txt" in*)
    (*let tokens : Lexer.token list = Lexer.tokenize_channel file in*)
    (*Lexer.print_tokens tokens;*)
    (*(*let ast : Ast.expr = Ast.generate_ast tokens in *)*)
    (*();;*)

let () = 
  print_string ">> "; flush stdout;
  let next_line = read_line () in
  let first_tokens = Lexer.tokenize_string next_line in

  (* run interpreter loop *)
  Toplevel.main_loop first_tokens

  
 
