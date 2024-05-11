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
    ( "e " >:: fun _ ->
      assert_equal true (close_enough Final_project.Arithmetic.e 2.7182818285)
    );
    ( "pow " >:: fun _ ->
      assert_equal true
        (close_enough Final_project.Arithmetic.(pow e 1.) 2.7182818285) );
  ]

let trigonometry_tests =
  [
    ( "Sine pi" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.sin Float.pi) 0.0) );
    ( "Sine pi/2" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.sin (Float.pi /. 2.)) 1.0) );
    ( "Sine 0" >:: fun _ ->
      assert_equal true (close_enough (Final_project.Trigonometry.sin 0.) 0.0)
    );
    ( "cos pi" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.cos Float.pi) ~-.1.0) );
    ( "cos pi/2" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.cos (Float.pi /. 2.)) 0.0) );
    ( "cos 0" >:: fun _ ->
      assert_equal true (close_enough (Final_project.Trigonometry.cos 0.) 1.0)
    );
    ( "tan pi" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.tan Float.pi) 0.0) );
    ( "tan pi/2" >:: fun _ ->
      assert_equal true
        (close_enough
           (Final_project.Trigonometry.tan (Float.pi /. 2.))
           (Float.tan (Float.pi /. 2.))) );
    ( "tan 0" >:: fun _ ->
      assert_equal true (close_enough (Final_project.Trigonometry.tan 0.) 0.0)
    );
    ( "arc Sine -1. -> -pi/2" >:: fun _ ->
      assert_equal true
        (close_enough
           (Final_project.Trigonometry.asin ~-.1.0)
           ~-.(Float.pi /. 2.)) );
    ( "arc Sine 1. -> pi/2" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.asin 1.) (Float.pi /. 2.)) );
    ( "arc Sine 0. -> 0" >:: fun _ ->
      assert_equal true (close_enough (Final_project.Trigonometry.asin 0.) 0.0)
    );
    ( "arc cosine -1. -> pi" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.acos ~-.1.0) Float.pi) );
    ( "arc cosine 1. -> 0" >:: fun _ ->
      assert_equal true (close_enough (Final_project.Trigonometry.acos 1.) 0.)
    );
    ( "arc cosine 0. -> pi/2" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.acos 0.) (Float.pi /. 2.)) );
    ( "arc tan -1. -> -pi/4" >:: fun _ ->
      assert_equal true
        (close_enough
           (Final_project.Trigonometry.atan ~-.1.0)
           ~-.(Float.pi /. 4.)) );
    ( "arc tan 1. -> pi/4" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Trigonometry.atan 1.) (Float.pi /. 4.)) );
    ( "arc tan 0. -> 0" >:: fun _ ->
      assert_equal true (close_enough (Final_project.Trigonometry.atan 0.) 0.)
    );
  ]

let math_tests =
  [
    ( "dec 0/denom = 0." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Math.dec (0, 10)) 0.) );
    ( "dec 1,5 = 0.2" >:: fun _ ->
      assert_equal true (close_enough (Final_project.Math.dec (1, 5)) 0.2) );
    ( "dec -1, 9 = -.111111111111" >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Math.dec (~-1, 9)) (~-.1. /. 9.)) );
    ( "cube 2. = 8." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Math.cube 2.) 8.) );
    ( "cube ~-.2. = ~-.8." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Math.cube ~-.2.) ~-.8.) );
    ( "cube 10. = 1000." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Math.cube 10.) 1000.) );
    ( "cube_root 8. = 2." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Math.cube_root 8.) 2.) );
    ( "cube_root ~-.8 = ~-.2." >:: fun _ ->
      assert_equal true
        (close_enough (Final_project.Math.cube_root ~-.8.) ~-.2.) );
    ( "cube_root (cube 2.) = 2." >:: fun _ ->
      assert_equal true
        (let n = 2. in
         close_enough
           (Final_project.Math.cube_root (Final_project.Math.cube n))
           n) );
    ( "cube_root (cube ~-.2.) = ~-.2." >:: fun _ ->
      assert_equal true
        (let n = ~-.2. in
         close_enough
           (Final_project.Math.cube_root (Final_project.Math.cube n))
           n) );
    ( "cube_root (cube ~-.100.) = ~-.100." >:: fun _ ->
      assert_equal true
        (let n = ~-.100. in
         close_enough
           (Final_project.Math.cube_root (Final_project.Math.cube n))
           n) );
    ( "n_root 8. = 2." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Math.n_root 3. 8.) 2.) );
    ( "frac 0.25 approx 0.25" >:: fun _ ->
      assert_equal true
        (let n = 0.25 in
         let num, denom = Final_project.Math.frac n in
         close_enough (float_of_int num /. float_of_int denom) n) );
    ( "frac 1.25 approx 1.25" >:: fun _ ->
      assert_equal true
        (let n = 1.25 in
         let num, denom = Final_project.Math.frac n in
         close_enough (float_of_int num /. float_of_int denom) n) );
    ( "frac 100 approx 100" >:: fun _ ->
      assert_equal true
        (let n = 100. in
         let num, denom = Final_project.Math.frac n in
         close_enough (float_of_int num /. float_of_int denom) n) );
    ( "frac Float.pi approx Float.pi" >:: fun _ ->
      assert_equal true
        (let n = Float.pi in
         let num, denom = Final_project.Math.frac n in
         close_enough (float_of_int num /. float_of_int denom) n) );
    ( "frac -0.5 approx -0.5" >:: fun _ ->
      assert_equal true
        (let n = ~-.0.5 in
         let num, denom = Final_project.Math.frac n in
         close_enough (float_of_int num /. float_of_int denom) n) );
    ( "frac .1111111111 approx .1111111111" >:: fun _ ->
      assert_equal true
        (let n = 0.1111111111 in
         let num, denom = Final_project.Math.frac n in
         close_enough (float_of_int num /. float_of_int denom) n) );
    ( "numerator and denominator of 0.25" >:: fun _ ->
      assert_equal (1, 4) (Final_project.Math.frac 0.25) );
    ( "numerator and denominator of 0.5" >:: fun _ ->
      assert_equal (1, 2) (Final_project.Math.frac 0.5) );
    ( "numerator and denominator of 0.75" >:: fun _ ->
      assert_equal (3, 4) (Final_project.Math.frac 0.75) );
    ( "numerator and denominator of 5.7" >:: fun _ ->
      assert_equal (57, 10) (Final_project.Math.frac 5.7) );
    ( "numerator and denominator of ~-.5.7" >:: fun _ ->
      assert_equal (~-57, 10) (Final_project.Math.frac ~-.5.7) );
  ]

let num_tests =
  [
    ("gcd 5 10 = 5" >:: fun _ -> assert_equal 5 (Final_project.Num.gcd 5 10));
    ("gcd 10 5 = 5" >:: fun _ -> assert_equal 5 (Final_project.Num.gcd 10 5));
    ("gcd 17 13 = 1" >:: fun _ -> assert_equal 1 (Final_project.Num.gcd 17 13));
    ("gcd 10 19 = 1" >:: fun _ -> assert_equal 1 (Final_project.Num.gcd 10 19));
    ("gcd 18 30 = 6" >:: fun _ -> assert_equal 6 (Final_project.Num.gcd 18 30));
    ( "abs (-x) = x int" >:: fun _ ->
      assert_equal (Final_project.Num.abs_i ~-10) 10 );
    ("abs (x) = x int" >:: fun _ -> assert_equal (Final_project.Num.abs_i 10) 10);
    ("abs (0) = 0 int" >:: fun _ -> assert_equal (Final_project.Num.abs_i 0) 0);
    ( "abs (-x) = x float" >:: fun _ ->
      assert_equal true (close_enough (Final_project.Num.abs_f ~-.10.) 10.) );
    ( "abs (x) = x float" >:: fun _ ->
      assert_equal true
        (let x = 10. in
         close_enough (Final_project.Num.abs_f x) x) );
    ( "abs (0) = 0 float" >:: fun _ ->
      assert_equal true
        (let x = 0. in
         close_enough (Final_project.Num.abs_f x) x) );
    ( "round 0.5 = 1." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Num.round 0.5) 1.) );
    ( "round 0.4 = 0." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Num.round 0.4) 0.) );
    ( "round ~-.0.499 = 0." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Num.round ~-.0.499) 0.) );
    ( "round ~-.0.6 = ~-.1." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Num.round ~-.0.6) ~-.1.) );
    ( "round ~-.0.5 = ~-.1." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Num.round ~-.0.5) ~-.1.) );
    ( "floor ~-.0.5 = ~-.1." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Num.floor ~-.0.5) ~-.1.) );
    ( "floor 0.5 = 0." >:: fun _ ->
      assert_equal true (close_enough (Final_project.Num.floor 0.5) 0.) );
    ( "remainder 3 / 4 = 3" >:: fun _ ->
      let num, denom = (3, 4) in
      let res = 3 in
      assert_equal res (Final_project.Num.remainder num denom) );
    ( "remainder 100 / 3 = 1" >:: fun _ ->
      let num, denom = (100, 3) in
      let res = 1 in
      assert_equal res (Final_project.Num.remainder num denom) );
    ( "remainder 123 / 18 = 15" >:: fun _ ->
      let num, denom = (123, 18) in
      let res = 15 in
      assert_equal res (Final_project.Num.remainder num denom) );
  ]

let parser_tests =
  [
    ( "query add ints" >:: fun _ ->
      assert_equal "2" (Final_project.Query.eval_query "1 + 1") );
    ( "query add negative ints" >:: fun _ ->
      assert_equal "-10" (Final_project.Query.eval_query "-11 + 1") );
    ( "query add two negative ints" >:: fun _ ->
      assert_equal "-12" (Final_project.Query.eval_query "-1 + -11") );
    ( "query add floats" >:: fun _ ->
      assert_equal "2.5" (Final_project.Query.eval_query "1.5 + 1.0") );
    ( "query add negative floats" >:: fun _ ->
      assert_equal "-10." (Final_project.Query.eval_query "-11.0 + 1") );
    ( "query add two negative floats" >:: fun _ ->
      assert_equal "-12.7" (Final_project.Query.eval_query "-1.2 + -11.5") );
    ( "query subtract ints" >:: fun _ ->
      assert_equal "2" (Final_project.Query.eval_query "3 - 1") );
    ( "query subtract negative ints" >:: fun _ ->
      assert_equal "-12" (Final_project.Query.eval_query "-11 - 1") );
    ( "query subtract two negative ints" >:: fun _ ->
      assert_equal "10" (Final_project.Query.eval_query "-1 - -11") );
    ( "query subtract floats" >:: fun _ ->
      assert_equal "1.7" (Final_project.Query.eval_query "3.0 - 1.3") );
    ( "query subtract negative floats" >:: fun _ ->
      assert_equal "-12." (Final_project.Query.eval_query "-11.0 - 1.0") );
    ( "query subtract two negative floats" >:: fun _ ->
      assert_equal "9.5" (Final_project.Query.eval_query "-1.5 - -11") );
    ( "query multiply ints" >:: fun _ ->
      assert_equal "3" (Final_project.Query.eval_query "3 * 1") );
    ( "query multiply negative ints" >:: fun _ ->
      assert_equal "-22" (Final_project.Query.eval_query "-11 * 2") );
    ( "query multiply two negatives ints" >:: fun _ ->
      assert_equal "11" (Final_project.Query.eval_query "-1 * -11") );
    ( "query multiply floats" >:: fun _ ->
      assert_equal "6.4" (Final_project.Query.eval_query "3.2 * 2") );
    ( "query multiply negative floats" >:: fun _ ->
      assert_equal "-2.25" (Final_project.Query.eval_query "-1.5 * 1.5") );
    ( "query multiply two negatives floats" >:: fun _ ->
      assert_equal "16.5" (Final_project.Query.eval_query "-1.5 * -11.0") );
    ( "query divide ints" >:: fun _ ->
      assert_equal "3" (Final_project.Query.eval_query "3 / 1") );
    ( "query divide negative ints" >:: fun _ ->
      assert_equal "-11" (Final_project.Query.eval_query "-23 / 2") );
    ( "query divide two negatives ints" >:: fun _ ->
      assert_equal "5" (Final_project.Query.eval_query "-11 / -2") );
    ( "query divide floats" >:: fun _ ->
      assert_equal "2." (Final_project.Query.eval_query "3.2 / 1.6") );
    ( "query divide negative floats" >:: fun _ ->
      assert_equal "-11." (Final_project.Query.eval_query "-22. / 2") );
    ( "query divide two negatives floats" >:: fun _ ->
      assert_equal "11." (Final_project.Query.eval_query "-5.5 / -0.5") );
    ( "query permute ints" >:: fun _ ->
      assert_equal "12" (Final_project.Query.eval_query "perm 4 2") );
    ( "query permute float and int" >:: fun _ ->
      assert_equal "12." (Final_project.Query.eval_query "perm 4. 2") );
    ( "query permute floats" >:: fun _ ->
      assert_equal "12." (Final_project.Query.eval_query "perm 4. 2.") );
    ( "query combination ints" >:: fun _ ->
      assert_equal "6" (Final_project.Query.eval_query "comb 4 2") );
    ( "query combination float and int" >:: fun _ ->
      assert_equal "6." (Final_project.Query.eval_query "comb 4. 2") );
    ( "query combination floats" >:: fun _ ->
      assert_equal "6." (Final_project.Query.eval_query "comb 4. 2.") );
    ( "query gcd ints" >:: fun _ ->
      assert_equal "5" (Final_project.Query.eval_query "gcd 5 15") );
    ( "query gcd ints" >:: fun _ ->
      assert_equal "5" (Final_project.Query.eval_query "gcd 5 -15") );
    ( "query gcd floats" >:: fun _ ->
      assert_equal "5." (Final_project.Query.eval_query "gcd 5. 15.") );
    ( "query gcd floats" >:: fun _ ->
      assert_equal "5." (Final_project.Query.eval_query "gcd 5. -15.") );
    ( "query remainder ints" >:: fun _ ->
      assert_equal "3" (Final_project.Query.eval_query "remainder 7 4") );
    ( "query remainder negative ints" >:: fun _ ->
      assert_equal "-10" (Final_project.Query.eval_query "remainder -23 13") );
    ( "query remainder floats" >:: fun _ ->
      assert_equal "3." (Final_project.Query.eval_query "remainder 7.1 4.") );
    ( "query remainder negative floats" >:: fun _ ->
      assert_equal "-10." (Final_project.Query.eval_query "remainder -23.1 13")
    );
    (* ( "query pow ints" >:: fun _ -> assert_equal "16"
       (Final_project.Query.eval_query "pow 2 4") ); ( "query pow negative ints"
       >:: fun _ -> assert_equal "-32" (Final_project.Query.eval_query "pow -2
       15") ); ( "query pow floats" >:: fun _ -> assert_equal "6.25"
       (Final_project.Query.eval_query "pow 2.5 2.") ); ( "query pow negative
       floats" >:: fun _ -> assert_equal "-27." (Final_project.Query.eval_query
       "pow -3. 3") ); *)
    (* ( "query nroot ints" >:: fun _ -> assert_equal "2"
       (Final_project.Query.eval_query "nroot 8 3") ); ( "query nroot negative
       ints" >:: fun _ -> assert_equal "-2" (Final_project.Query.eval_query
       "nroot -8 3") ); ( "query nroot floats" >:: fun _ -> assert_equal "3."
       (Final_project.Query.eval_query "nroot 27. 3.") ); ( "query nroot
       negative floats" >:: fun _ -> assert_equal "-1."
       (Final_project.Query.eval_query "nroot -1. 13") ); *)
    ( "query invert ints" >:: fun _ ->
      assert_equal "0.5" (Final_project.Query.eval_query "inv 2") );
    ( "query invert negative ints" >:: fun _ ->
      assert_equal "-0.5" (Final_project.Query.eval_query "inv -2") );
    ( "query invert floats" >:: fun _ ->
      assert_equal "0.5" (Final_project.Query.eval_query "inv 2.") );
    ( "query invert negative floats" >:: fun _ ->
      assert_equal "-0.5" (Final_project.Query.eval_query "inv -2.") );
    ( "query ln e" >:: fun _ ->
      assert_equal "1." (Final_project.Query.eval_query "ln e") );
    ( "query log 10. float" >:: fun _ ->
      assert_equal "1." (Final_project.Query.eval_query "log 10.") );
    ( "query log 10 int" >:: fun _ ->
      assert_equal "1." (Final_project.Query.eval_query "log 10") );
    ( "query square ints" >:: fun _ ->
      assert_equal "4" (Final_project.Query.eval_query "square 2") );
    ( "query square negative ints" >:: fun _ ->
      assert_equal "4" (Final_project.Query.eval_query "square -2") );
    ( "query square floats" >:: fun _ ->
      assert_equal "4." (Final_project.Query.eval_query "square 2.") );
    ( "query square negative floats" >:: fun _ ->
      assert_equal "4." (Final_project.Query.eval_query "square -2.") );
    ( "query cube ints" >:: fun _ ->
      assert_equal "8" (Final_project.Query.eval_query "cube 2") );
    ( "query cube negative ints" >:: fun _ ->
      assert_equal "-8" (Final_project.Query.eval_query "cube -2") );
    ( "query cube floats" >:: fun _ ->
      assert_equal "8." (Final_project.Query.eval_query "cube 2.") );
    ( "query cube negative floats" >:: fun _ ->
      assert_equal "-8." (Final_project.Query.eval_query "cube -2.") );
    ( "query tenx 2. float" >:: fun _ ->
      assert_equal "100." (Final_project.Query.eval_query "tenx 2.") );
    ( "query tenx -1. float" >:: fun _ ->
      assert_equal "0.1" (Final_project.Query.eval_query "tenx -1.") );
    ( "query tenx 2 int" >:: fun _ ->
      assert_equal "100." (Final_project.Query.eval_query "tenx 2") );
    ( "query tenx 0 int" >:: fun _ ->
      assert_equal "1." (Final_project.Query.eval_query "tenx 0") );
    ( "query sqrt ints" >:: fun _ ->
      assert_equal "2." (Final_project.Query.eval_query "sqrt 4") );
    ( "query sqrt floats" >:: fun _ ->
      assert_equal "2." (Final_project.Query.eval_query "sqrt 4.") );
    ( "query exp ints" >:: fun _ ->
      assert_equal "2.71828182846" (Final_project.Query.eval_query "exp 1") );
    ( "query exp floats" >:: fun _ ->
      assert_equal "2.71828182846" (Final_project.Query.eval_query "exp 1.") );
    ( "query exp 0 " >:: fun _ ->
      assert_equal "1." (Final_project.Query.eval_query "exp 0") );
    ( "query sin 0 float" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "sin 0.") );
    ( "query sin 0 int" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "sin 0") );
    ( "query cos 0 float" >:: fun _ ->
      assert_equal "1." (Final_project.Query.eval_query "cos 0.") );
    ( "query cos 0 int" >:: fun _ ->
      assert_equal "1." (Final_project.Query.eval_query "cos 0") );
    ( "query tan 0. float" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "tan 0.") );
    ( "query tan 0 int" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "tan 0") );
    ( "query asin 0 float" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "asin 0.") );
    ( "query asin 0 int" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "asin 0") );
    ( "query acos 1. float" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "acos 1.") );
    ( "query acos 1 int" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "acos 1") );
    ( "query atan 0. float" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "atan 0.") );
    ( "query atan 0 int" >:: fun _ ->
      assert_equal "0." (Final_project.Query.eval_query "atan 0") );
    ( "query fact 5 int" >:: fun _ ->
      assert_equal "120" (Final_project.Query.eval_query "fact 5") );
    ( "query fact 5. float" >:: fun _ ->
      assert_equal "120." (Final_project.Query.eval_query "fact 5.") );
    ( "query abs -5 int" >:: fun _ ->
      assert_equal "5" (Final_project.Query.eval_query "abs -5") );
    ( "query abs -5. float" >:: fun _ ->
      assert_equal "5." (Final_project.Query.eval_query "abs -5.") );
    ( "query round 5 int" >:: fun _ ->
      assert_equal "5" (Final_project.Query.eval_query "round 5") );
    ( "query round 5.51 float" >:: fun _ ->
      assert_equal "6." (Final_project.Query.eval_query "round 5.51") );
    ( "query round -5.51 float" >:: fun _ ->
      assert_equal "-6." (Final_project.Query.eval_query "round -5.51") );
    ( "query floor 5 int" >:: fun _ ->
      assert_equal "5" (Final_project.Query.eval_query "floor 5") );
    ( "query floor 5.51 float" >:: fun _ ->
      assert_equal "5." (Final_project.Query.eval_query "floor 5.51") );
    ( "query floor -5.51 float" >:: fun _ ->
      assert_equal "-6." (Final_project.Query.eval_query "floor -5.51") );
  ]

let tests = [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]

let suite =
  "sort test suite"
  >::: List.flatten
         [
           probability_tests;
           arithmetic_tests;
           trigonometry_tests;
           math_tests;
           num_tests;
           parser_tests;
           tests;
         ]

let _ = run_test_tt_main suite
