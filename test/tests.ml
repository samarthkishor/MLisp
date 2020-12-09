let () =
  let open Alcotest in
  let lexical_closures_tests =
    [ test_case "lexical closures" `Quick Extra_tests.test_closures ]
  in
  let core_functions_tests =
    [
      QCheck_alcotest.to_alcotest Core_functions_tests.test_apply;
      test_case "apply (2)" `Quick Core_functions_tests.test_apply_error;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_append_lists;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_append_strings;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_append_numbers;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_car;
      test_case "car (2)" `Quick Core_functions_tests.test_car_empty;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_cdr;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_cons;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_expt;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_length;
      test_case "list?" `Quick Core_functions_tests.test_is_list;
      test_case "map" `Quick Core_functions_tests.test_map;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_max;
      QCheck_alcotest.to_alcotest Core_functions_tests.test_min;
      test_case "null?" `Quick Core_functions_tests.test_is_null;
      test_case "number?" `Quick Core_functions_tests.test_is_number;
      test_case "procedure?" `Quick Core_functions_tests.test_is_procedure;
    ]
  in
  let refactor_tests =
    List.map QCheck_alcotest.to_alcotest
      [
        Refactor_tests.test_add;
        Refactor_tests.test_sub;
        Refactor_tests.test_mult;
        Refactor_tests.test_div;
        Refactor_tests.test_gt;
        Refactor_tests.test_lt;
        Refactor_tests.test_ge;
        Refactor_tests.test_le;
        Refactor_tests.test_gt_string;
        Refactor_tests.test_lt_string;
        Refactor_tests.test_ge_string;
        Refactor_tests.test_le_string;
      ]
  in
  let float_tests =
    List.map QCheck_alcotest.to_alcotest
      [
        Float_tests.test_expt_float_1;
        Float_tests.test_expt_float_2;
        Float_tests.test_expt_float_3;
        Float_tests.test_max_float_1;
        Float_tests.test_max_float_2;
        Float_tests.test_max_float_3;
        Float_tests.test_min_float_1;
        Float_tests.test_min_float_2;
        Float_tests.test_min_float_3;
        Float_tests.test_add_float_1;
        Float_tests.test_add_float_2;
        Float_tests.test_add_float_3;
        Float_tests.test_sub_float_1;
        Float_tests.test_sub_float_2;
        Float_tests.test_sub_float_3;
        Float_tests.test_mult_float_1;
        Float_tests.test_mult_float_2;
        Float_tests.test_mult_float_3;
        Float_tests.test_div_float_1;
        Float_tests.test_div_float_2;
        Float_tests.test_div_float_3;
        Float_tests.test_gt_float_1;
        Float_tests.test_gt_float_2;
        Float_tests.test_gt_float_3;
        Float_tests.test_lt_float_1;
        Float_tests.test_lt_float_2;
        Float_tests.test_lt_float_3;
        Float_tests.test_ge_float_1;
        Float_tests.test_ge_float_2;
        Float_tests.test_ge_float_3;
        Float_tests.test_le_float_1;
        Float_tests.test_le_float_2;
        Float_tests.test_le_float_3;
      ]
  in
  run "MLisp tests"
    [
      ("1. Lexical closures", lexical_closures_tests);
      ("2. Core functions", core_functions_tests);
      ("3. Refactor", refactor_tests);
      ("4. Float support", float_tests);
      ( "5. Round",
        [ QCheck_alcotest.to_alcotest Core_functions_tests.test_round ] );
      ( "6. Boolean and/or",
        List.map QCheck_alcotest.to_alcotest
          [ Core_functions_tests.test_and; Core_functions_tests.test_or ] );
      ( "?. Extra",
        List.map QCheck_alcotest.to_alcotest
          [
            Extra_tests.test_x_times_x;
            Extra_tests.test_lambda;
            Extra_tests.test_recursion;
          ] );
    ]
