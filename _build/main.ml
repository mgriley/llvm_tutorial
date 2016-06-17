
let () = 
    let tokens : Lexer.token list = Lexer.tokenize_file () in
    Lexer.print_tokens tokens;
    (*let ast : Ast.expr = Ast.generate_ast tokens in *)
    ();;
