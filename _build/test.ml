open OUnit2;;

let test1 test_ctxt = assertEquals true true;;

let suite = 
  "suite">:::
    ["test 1">:: test1]
;;

let () =
  run_test_tt_main suite
;;
