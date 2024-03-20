open OUnit2

let probability_tests =
  [
    ( "factorial 1." >:: fun _ ->
      assert_equal 1. (Final_project.Probability.factorial 1.) );
    ( "factorial 4." >:: fun _ ->
      assert_equal 24. (Final_project.Probability.factorial 4.) );
  ]

let tests = [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]
let suite = "sort test suite" >::: List.flatten [ probability_tests; tests ]
let _ = run_test_tt_main suite
