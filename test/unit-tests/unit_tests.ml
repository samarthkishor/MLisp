let test_apply () =
  let res =
    MLisp.(
      eval global_env
        (List
           [
             Symbol "apply"; Symbol "+"; List [ Symbol "list"; Int 1L; Int 2L ];
           ]))
  in
  match res with
  | Int n -> Alcotest.(check int64) "same int" 3L n
  | _ -> Alcotest.fail "apply failed"

let test_car_empty () =
  let res =
    try
      MLisp.(
        eval global_env
          (List [ Symbol "car"; List [ Symbol "quote"; List [] ] ]))
    with
    | MLisp.Runtime_error _ -> Bool true
    | _ -> Bool false
  in
  match res with
  | Bool b -> Alcotest.(check bool) "raises error" true b
  | _ -> Alcotest.fail "car failed"

let test_map () =
  let res =
    MLisp.(
      eval global_env
        (List
           [
             Symbol "map";
             Symbol "not";
             List [ Symbol "list"; Bool true; Bool false ];
           ]))
  in
  match res with
  | List lst ->
      Alcotest.(check (list bool))
        "same list" [ false; true ]
        (List.map MLisp.Type.to_bool lst)
  | _ -> Alcotest.fail "map failed"

let test_is_null () =
  let r1, r2 =
    ( MLisp.(eval global_env (List [ Symbol "null?"; List [ Symbol "list" ] ])),
      MLisp.(
        eval global_env
          (List [ Symbol "null?"; List [ Symbol "list"; Int 1L ] ])) )
  in
  match (r1, r2) with
  | Bool b1, Bool b2 ->
      let () = Alcotest.(check bool) "same bool" true b1 in
      Alcotest.(check bool) "same bool" false b2
  | _ -> Alcotest.fail "null? failed"

let test_is_list () =
  let r1, r2 =
    ( MLisp.(eval global_env (List [ Symbol "list?"; List [ Symbol "list" ] ])),
      MLisp.(
        eval global_env
          (List [ Symbol "list?"; List [ Symbol "list?"; Int 1L ] ])) )
  in
  match (r1, r2) with
  | Bool b1, Bool b2 ->
      let () = Alcotest.(check bool) "same bool" true b1 in
      Alcotest.(check bool) "same bool" false b2
  | _ -> Alcotest.fail "list? failed"

let test_is_number () =
  let r1, r11, r2 =
    ( MLisp.(eval global_env (List [ Symbol "number?"; Int 1L ])),
      MLisp.(eval global_env (List [ Symbol "number?"; Float 1. ])),
      MLisp.(
        eval global_env
          (List [ Symbol "number?"; List [ Symbol "list"; Int 1L ] ])) )
  in
  match (r1, r11, r2) with
  | Bool b1, Bool b11, Bool b2 ->
      let () = Alcotest.(check bool) "same bool" true b1 in
      let () = Alcotest.(check bool) "same bool" true b11 in
      Alcotest.(check bool) "same bool" false b2
  | _ -> Alcotest.fail "number? failed"

let test_is_procedure () =
  let r1, r2 =
    ( MLisp.(eval global_env (List [ Symbol "procedure?"; Symbol "+" ])),
      MLisp.(
        eval global_env
          (List [ Symbol "procedure?"; List [ Symbol "list"; Int 1L ] ])) )
  in
  match (r1, r2) with
  | Bool b1, Bool b2 ->
      let () = Alcotest.(check bool) "same bool" true b1 in
      Alcotest.(check bool) "same bool" false b2
  | _ -> Alcotest.fail "procedure? failed"

let test_apply_error () =
  try
    let _ =
      MLisp.(
        eval global_env
          (List
             [
               Symbol "apply";
               Symbol "+";
               List [ Symbol "list"; Int 1L; Int 2L; Int 3L ];
             ]))
    in
    Alcotest.fail "apply should have raised a Runtime_error"
  with
  | MLisp.Runtime_error _ -> Alcotest.(check bool) "pass" true true
  | _ -> Alcotest.fail "apply should have raised a Runtime_error"

let test_closures () =
  let res =
    MLisp.(
      eval global_env
        (List
           [
             Symbol "begin";
             List
               [
                 Symbol "define";
                 Symbol "make-counter";
                 List
                   [
                     Symbol "lambda";
                     List [];
                     List
                       [
                         Symbol "begin";
                         List [ Symbol "define"; Symbol "count"; Int 0L ];
                         List
                           [
                             Symbol "lambda";
                             List [];
                             List
                               [
                                 Symbol "begin";
                                 List
                                   [
                                     Symbol "set!";
                                     Symbol "count";
                                     List [ Symbol "+"; Symbol "count"; Int 1L ];
                                   ];
                                 Symbol "count";
                               ];
                           ];
                       ];
                   ];
               ];
             List
               [
                 Symbol "begin";
                 List
                   [
                     Symbol "define";
                     Symbol "counter1";
                     List [ Symbol "make-counter" ];
                   ];
                 List
                   [
                     Symbol "+";
                     List [ Symbol "counter1" ];
                     List [ Symbol "counter1" ];
                   ];
               ];
           ]))
  in
  match res with
  | Int n -> Alcotest.(check int64) "same int" (Int64.add 1L 2L) n
  | _ -> Alcotest.fail "type error"

let () =
  let open Alcotest in
  let added_funs_unit_tests =
    [
      test_case "apply" `Quick test_apply;
      test_case "car of empty list" `Quick test_car_empty;
      test_case "map" `Quick test_map;
      test_case "list?" `Quick test_is_list;
      test_case "null?" `Quick test_is_null;
      test_case "number?" `Quick test_is_number;
      test_case "procedure?" `Quick test_is_procedure;
    ]
  in
  let mlisp_program_unit_tests =
    [
      test_case "(apply + (list 1 2 3))" `Quick test_apply_error;
      test_case
        "(begin (define make-counter (lambda () (begin (define count 0) \
         (lambda () (begin (set! count (+ count 1)) count))))) (begin (define \
         counter1 (make-counter)) (+ (counter1) (counter1))))"
        `Quick test_closures;
    ]
  in
  run "MLisp unit tests"
    [
      ("Core functions unit tests", added_funs_unit_tests);
      ("MLisp program unit tests", mlisp_program_unit_tests);
    ]
