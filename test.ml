open OUnit2;;

(* Utilities *)

let string_of_tokens (tokens : Lexer.token list) : string =
  let combine = fun acc token -> 
    acc ^ " " ^ (Lexer.string_of_token token) in
  List.fold_left combine "" tokens 
;;

let get_test_file (contents: string) (test_ctxt) : string =
  let (file_name, output_channel) = bracket_tmpfile test_ctxt in
  output_string output_channel contents; 
  flush output_channel;
  file_name
;;

let token_check (exp: Lexer.token list) (actual: Lexer.token list) (test_ctxt) : unit =
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens exp actual; ()
;;

(* Lexer tests *)

let empty_file_test test_ctxt = 
  let file_name = get_test_file "" test_ctxt in
  token_check [] (Lexer.tokenize_file file_name) test_ctxt
;;

let num_test test_ctxt = 
  let file_name = get_test_file "3.4" test_ctxt in 
  token_check [Lexer.Number 3.4] (Lexer.tokenize_file file_name) test_ctxt
;;

let def test_ctxt = 
  let file_name = get_test_file "def" test_ctxt in
  token_check [Lexer.Def] (Lexer.tokenize_file file_name) test_ctxt
;;

let extern test_ctxt = 
  let file_name = get_test_file "extern" test_ctxt in
  token_check [Lexer.Extern] (Lexer.tokenize_file file_name) test_ctxt
;;

let ident test_ctxt = 
  let file_name = get_test_file "foo" test_ctxt in
  token_check [Lexer.Ident "foo"] (Lexer.tokenize_file file_name) test_ctxt
;;

let keyword test_ctxt = 
  let file_name = get_test_file "(" test_ctxt in
  token_check [Lexer.Kwd '('] (Lexer.tokenize_file file_name) test_ctxt
;;

let comment test_ctxt = 
  let file_name = get_test_file "# ignore this comment\ndef" test_ctxt in
  token_check [Lexer.Def] (Lexer.tokenize_file file_name) test_ctxt
;;

let zero_ending_num test_ctxt = 
  let file_name = get_test_file "3.0" test_ctxt in
  token_check [Lexer.Number 3.0] (Lexer.tokenize_file file_name) test_ctxt
;;

let ident_and_kwd test_ctxt = 
  let file_name = get_test_file "(foo)" test_ctxt in
  token_check [Lexer.Kwd '('; Lexer.Ident "foo"; Lexer.Kwd ')'] (Lexer.tokenize_file file_name) test_ctxt
;;

let num_parens test_ctxt = 
  let file_name = get_test_file "(2)" test_ctxt in
  token_check [Lexer.Kwd '('; Lexer.Number 2.0; Lexer.Kwd ')'] (Lexer.tokenize_file file_name) test_ctxt
;;

let ident_with_trailing_num test_ctxt = 
  let file_name = get_test_file "abc1" test_ctxt in
  token_check [Lexer.Ident "abc1"] (Lexer.tokenize_file file_name) test_ctxt
;;

(* TODO: raise a compiler error in this instance, there must be a space b/w them *)
let ident_with_leading_num test_ctxt = 
  let file_name = get_test_file "2chainz" test_ctxt in
  token_check [Lexer.Number 2.0; Lexer.Ident "chainz"] (Lexer.tokenize_file file_name) test_ctxt 
;;

let def_with_parens test_ctxt =
  let file_name = get_test_file "def foo()" test_ctxt in
  token_check [Lexer.Def; Lexer.Ident "foo"; Lexer.Kwd '('; Lexer.Kwd ')'] (Lexer.tokenize_file file_name) test_ctxt

(* AST Tests *)

let parse_proto_noargs test_ctxt = 
  let tokens = [Lexer.Ident "sq"; Lexer.Kwd '('; Lexer.Kwd ')'; Lexer.Ident "foo"] in
  let (proto, remaining) = Ast.parse_prototype tokens in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_proto (Ast.Prototype ("sq", [||])) proto;
  token_check [Lexer.Ident "foo"] remaining test_ctxt
;;

let parse_proto_single_arg test_ctxt = 
  let tokens = [Lexer.Ident "sq"; Lexer.Kwd '('; Lexer.Ident "my_var"; Lexer.Kwd ')'; Lexer.Ident "foo"] in
  let (proto, remaining) = Ast.parse_prototype tokens in
  assert_equal ~ctxt: test_ctxt ~printer: Ast.string_of_proto (Ast.Prototype ("sq", [|"my_var"|])) proto;   
  token_check [Lexer.Ident "foo"] remaining test_ctxt
;;

let parse_proto_multiple_args test_ctxt = 
  let tokens = [Lexer.Ident "sq"; Lexer.Kwd '('; Lexer.Ident "a"; Lexer.Ident "b"; Lexer.Kwd ')'; Lexer.Ident "foo"] in
  let (proto, remaining) = Ast.parse_prototype tokens in
  assert_equal ~ctxt: test_ctxt ~printer: Ast.string_of_proto (Ast.Prototype ("sq", [|"a"; "b"|])) proto;   
  token_check [Lexer.Ident "foo"] remaining test_ctxt
;;

(* parse function *)

let parse_func_noargs test_ctxt = 
  let tokens = [Lexer.Def; Lexer.Ident "foo"; Lexer.Kwd '('; Lexer.Kwd ')'; Lexer.Number 1.0] in
  let (func, remaining) = Ast.parse_function tokens in
  let proto = Ast.Prototype ("foo", [||]) in
  assert_equal ~ctxt:test_ctxt ~printer: Ast.string_of_func (Ast.Function (proto, Ast.Number 1.0)) func
;;

(* parse function calls *)

let parse_call_one_arg test_ctxt =
  let tokens = [Lexer.Ident "one"; Lexer.Kwd '('; Lexer.Number 1.0; Lexer.Kwd ')'] in
  let (call, remaining) = Ast.parse_call tokens in
  let expected = Ast.Call ("one", [|Ast.Number 1.0|]) in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected call
;;

let parse_call_two_args test_ctxt =
  let tokens = [Lexer.Ident "one"; Lexer.Kwd '('; Lexer.Number 1.0; Lexer.Kwd ','; Lexer.Number 2.0; Lexer.Kwd ')'] in
  let (call, remaining) = Ast.parse_call tokens in
  let expected = Ast.Call ("one", [|Ast.Number 1.0; Ast.Number 2.0|]) in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected call
;;

(* Binary expression parsing *)

let parse_single_expr test_ctxt =
  let tokens = [Lexer.Number 1.0; Lexer.Ident "ha"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr (Ast.Number 1.0) expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "ha"] remaining
;;

let parse_two_term_expr test_ctxt = 
  let tokens = [Lexer.Ident "a"; Lexer.Kwd '+'; Lexer.Ident "b"; Lexer.Ident "c"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  let expected = Ast.Binary ('+', Ast.Variable "a", Ast.Variable "b") in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "c"] remaining
;;

let three_increasing_prec test_ctxt =
  let tokens = [Lexer.Ident "a"; Lexer.Kwd '+'; Lexer.Ident "b"; Lexer.Kwd '*'; Lexer.Ident "c"; Lexer.Ident "d"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  let expected = Ast.Binary('+', Ast.Variable "a", Ast.Binary ('*', Ast.Variable "b", Ast.Variable "c")) in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "d"] remaining

let three_decreasing_prec test_ctxt =
  let tokens = [Lexer.Ident "a"; Lexer.Kwd '*'; Lexer.Ident "b"; Lexer.Kwd '+'; Lexer.Ident "c"; Lexer.Ident "d"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  let expected = Ast.Binary('+', Ast.Binary('*', Ast.Variable "a", Ast.Variable "b"), Ast.Variable "c") in 
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "d"] remaining

let three_same_prec test_ctxt =
  let tokens = [Lexer.Ident "a"; Lexer.Kwd '+'; Lexer.Ident "b"; Lexer.Kwd '+'; Lexer.Ident "c"; Lexer.Ident "d"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  let expected = Ast.Binary('+', Ast.Variable "a", Ast.Binary('+', Ast.Variable "b", Ast.Variable "c")) in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "d"] remaining

let binary_high_plateau test_ctxt = 
  let tokens = [Lexer.Ident "a"; Lexer.Kwd '+'; Lexer.Ident "b"; Lexer.Kwd '*'; Lexer.Ident "c"; Lexer.Kwd '*'; Lexer.Ident "d"; Lexer.Ident "foo"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  let expected = Ast.Binary('+', Ast.Variable "a", Ast.Binary('*', Ast.Variable "b", Ast.Binary('*', Ast.Variable "c", Ast.Variable "d"))) in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "foo"] remaining

let binary_low_plateau test_ctxt = 
  let tokens = [Lexer.Ident "a"; Lexer.Kwd '*'; Lexer.Ident "b"; Lexer.Kwd '+'; Lexer.Ident "c"; Lexer.Kwd '+'; Lexer.Ident "d"; Lexer.Ident "foo"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  let expected = Ast.Binary('+', Ast.Binary('*', Ast.Variable "a", Ast.Variable "b"), Ast.Binary('+', Ast.Variable "c", Ast.Variable "d")) in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "foo"] remaining

let binary_high_peak test_ctxt = 
  let tokens = [Lexer.Ident "a"; Lexer.Kwd '+'; Lexer.Ident "b"; Lexer.Kwd '*'; Lexer.Ident "c"; Lexer.Kwd '+'; Lexer.Ident "d"; Lexer.Ident "foo"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  let expected = Ast.Binary('+', Ast.Variable "a", Ast.Binary('+', Ast.Binary('*', Ast.Variable "b", Ast.Variable "c"), Ast.Variable "d")) in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "foo"] remaining

let binary_low_peak test_ctxt = 
  let tokens = [Lexer.Ident "a"; Lexer.Kwd '*'; Lexer.Ident "b"; Lexer.Kwd '+'; Lexer.Ident "c"; Lexer.Kwd '*'; Lexer.Ident "d"; Lexer.Ident "foo"] in
  let (expr, remaining) = Ast.parse_expr tokens in
  let expected = Ast.Binary ('+', Ast.Binary('*', Ast.Variable "a", Ast.Variable "b"), Ast.Binary('*', Ast.Variable "c", Ast.Variable "d")) in
  assert_equal ~ctxt:test_ctxt ~printer:Ast.string_of_expr expected expr;
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens [Lexer.Ident "foo"] remaining

let suite = 
  "suite">:::
    [
      "empty file test" >:: empty_file_test;
      "single num test" >:: num_test;
      "single def test" >:: def;
      "single extern test" >:: extern;
      "single ident test" >:: ident;
      "single keyword test" >:: keyword;
      "ignore comment line" >:: comment;
      "num ending in zero" >:: zero_ending_num;
      "ident in parens" >:: ident_and_kwd;
      "number in parens" >:: num_parens;
      "ident with trailing num" >:: ident_with_trailing_num;
      "ident with leading num" >:: ident_with_leading_num;
      "function def no spaces until parens" >:: def_with_parens;
      "parse proto no args" >:: parse_proto_noargs;
      "parse proto one arg" >:: parse_proto_single_arg;
      "parse proto multi args" >:: parse_proto_multiple_args;
      "parse func no args" >:: parse_func_noargs;
      "parse call one arg" >:: parse_call_one_arg;
      "parse call two args" >:: parse_call_two_args;
      "parse binary expr single term" >:: parse_single_expr;
      "parse binary expr two terms" >:: parse_two_term_expr;
      "parse binary expr. three terms, increasing precedence" >:: three_increasing_prec;
      "parse binary expr. three terms, decreasing precedence" >:: three_decreasing_prec;
      "parse binary expr. three terms, increasing precedence" >:: three_same_prec;
      "parse binary expr, high plateau" >:: binary_high_plateau;
      "parse binary expr, low plateau" >:: binary_low_plateau;
      "parse binary expr, high peak" >:: binary_high_peak;
      "parse binary expr, low peak" >:: binary_low_peak
    ];
;;

let () =
  run_test_tt_main suite
;;
