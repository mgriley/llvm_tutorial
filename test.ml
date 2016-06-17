open OUnit2;;

(* Utilities *)

let string_of_tokens (tokens : Lexer.token list) : string =
  let combine = fun acc token -> 
    acc ^ (Lexer.string_of_token token) in
  List.fold_left combine "" tokens 
;;

let get_test_file (contents: string) (test_ctxt) : string =
  let (file_name, output_channel) = bracket_tmpfile test_ctxt in
  output_string output_channel contents; flush output_channel;
  file_name
;;

let token_check (exp: Lexer.token list) (actual: Lexer.token list) test_ctxt : unit =
  assert_equal ~ctxt:test_ctxt ~printer:string_of_tokens exp actual; ()
;;

(* Tests *)

let empty_file_test test_ctxt = 
  let file_name = get_test_file "" test_ctxt in
  token_check [] (Lexer.tokenize_file file_name)
;;

let num_test test_ctxt = 
  let file_name = get_test_file "3.4" test_ctxt in 
  token_check [Lexer.Number 3.4] (Lexer.tokenize_file file_name)
;;

let def test_ctxt = 
  let file_name = get_test_file "def" test_ctxt in
  token_check [Lexer.Def] (Lexer.tokenize_file file_name)
;;

let suite = 
  "suite">:::
    [
      "empty file test" >:: empty_file_test;
      "single num test" >:: num_test;
      "single def test" >:: def
    ];
;;

let () =
  run_test_tt_main suite
;;
