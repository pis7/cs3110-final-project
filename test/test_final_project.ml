open OUnit2

let probability_tests =
  [
    ( "factorial 1." >:: fun _ ->
      assert_equal 1. (Final_project.Probability.factorial 1.) );
    ( "factorial 4." >:: fun _ ->
      assert_equal 24. (Final_project.Probability.factorial 4.) );
    ( "rand 1." >:: fun _ ->
      assert_equal true
        (let num = Final_project.Probability.rand 1. in
         match num with
         | [] -> false
         | h :: _ -> 0. <= h && h <= 1.) );
    ( "rand 2." >:: fun _ ->
      assert_equal true
        (let num = Final_project.Probability.rand 2. in
         match num with
         | h :: h2 :: _ -> 0. <= h && h <= 1. && 0. <= h2 && h2 <= 1.
         | _ -> false) );
    ( "permutation 4. 2." >:: fun _ ->
      assert_equal 12. (Final_project.Probability.permutation 4. 2.) );
    ( "combination 4. 2." >:: fun _ ->
      assert_equal 6. (Final_project.Probability.combination 4. 2.) );
  ]

let tests = [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]
let suite = "sort test suite" >::: List.flatten [ probability_tests; tests ]
let _ = run_test_tt_main suite
