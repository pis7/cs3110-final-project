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

let close_enough a b = Float.abs (a -. b) < 0.00000001

let arithmetic_tests =
  [
    ( "add_i " >:: fun _ ->
      assert_equal 2 (Final_project.Arithmetic.add_i 1 1) ~printer:string_of_int
    );
    ( "sub_i " >:: fun _ ->
      assert_equal 1 (Final_project.Arithmetic.sub_i 2 1) ~printer:string_of_int
    );
    ( "mult_i " >:: fun _ ->
      assert_equal 2
        (Final_project.Arithmetic.mult_i 2 1)
        ~printer:string_of_int );
    ( "div_i " >:: fun _ ->
      assert_equal 2 (Final_project.Arithmetic.div_i 4 2) ~printer:string_of_int
    );
    ( "add " >:: fun _ ->
      assert_equal 2.
        (Final_project.Arithmetic.add_f 1. 1.)
        ~printer:string_of_float );
    ( "sub " >:: fun _ ->
      assert_equal 1.
        (Final_project.Arithmetic.sub_f 2. 1.)
        ~printer:string_of_float );
    ( "mult " >:: fun _ ->
      assert_equal 2.
        (Final_project.Arithmetic.mult_f 2. 1.)
        ~printer:string_of_float );
    ( "div " >:: fun _ ->
      assert_equal 2.
        (Final_project.Arithmetic.div_f 4. 2.)
        ~printer:string_of_float );
    ( "inverse " >:: fun _ ->
      assert_equal 2.
        (Final_project.Arithmetic.inverse 0.5)
        ~printer:string_of_float );
    ( "square " >:: fun _ ->
      assert_equal 4.
        (Final_project.Arithmetic.square 2.)
        ~printer:string_of_float );
    ( "sqrt " >:: fun _ ->
      assert_equal 2.
        (Final_project.Arithmetic.sqrt 4.)
        ~printer:string_of_float );
    ( "log " >:: fun _ ->
      assert_equal 2.
        (Final_project.Arithmetic.log 100.)
        ~printer:string_of_float );
    ( "ten_x " >:: fun _ ->
      assert_equal 100.
        (Final_project.Arithmetic.ten_x 2.)
        ~printer:string_of_float );
    ( "ln " >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Arithmetic.ln 2.) 0.6931471806) );
    ( "exp " >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Arithmetic.exp 2.) 7.3890560989) );
    ( "pi " >:: fun _ ->
      assert_equal Float.pi Final_project.Arithmetic.pi ~printer:string_of_float
    );
    ( "exp " >:: fun _ ->
      assert_equal true (close_enough Final_project.Arithmetic.e 2.7182818285)
    );
  ]

let tests = [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]

let suite =
  "sort test suite"
  >::: List.flatten [ probability_tests; arithmetic_tests; tests ]

let _ = run_test_tt_main suite
