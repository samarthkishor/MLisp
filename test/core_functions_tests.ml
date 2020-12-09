let test_apply =
  QCheck.Test.make ~count:100 ~name:"apply (1)"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "apply";
                 Symbol "+";
                 List [ Symbol "list"; Int a; Int b ];
               ]))
      in
      match res with Int n -> Int64.equal n (Int64.add a b) | _ -> false)

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

let test_append_lists =
  QCheck.Test.make ~count:100 ~name:"append lists"
    QCheck.(pair (small_list int64) (small_list int64))
    (fun (l1, l2) ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "append";
                 List (Symbol "list" :: List.map (fun n -> Type.Int n) l1);
                 List (Symbol "list" :: List.map (fun n -> Type.Int n) l2);
               ]))
      in
      match res with
      | List l -> List.map MLisp.Type.to_int l = l1 @ l2
      | _ -> false)

let test_append_strings =
  QCheck.Test.make ~count:100 ~name:"append strings"
    QCheck.(pair printable_string printable_string)
    (fun (s1, s2) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "append"; String s1; String s2 ]))
      in
      match res with String s -> String.equal s (s1 ^ s2) | _ -> false)

let test_append_numbers =
  QCheck.Test.make ~count:100 ~name:"append numbers"
    QCheck.(pair int64 int64)
    (fun (n1, n2) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "append"; Int n1; Int n2 ]))
      in
      match res with Int n -> Int64.equal n (Int64.add n1 n2) | _ -> false)

let test_car =
  QCheck.Test.make ~count:100 ~name:"car (1)"
    QCheck.(small_list int64)
    (fun lst ->
      let () = QCheck.assume (List.length lst > 0) in
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "car";
                 List (Symbol "list" :: List.map (fun n -> Type.Int n) lst);
               ]))
      in
      match res with Int n -> Int64.equal n (List.hd lst) | _ -> false)

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

let test_cdr =
  QCheck.Test.make ~count:100 ~name:"cdr"
    QCheck.(small_list int64)
    (fun lst ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "cdr";
                 List (Symbol "list" :: List.map (fun n -> Type.Int n) lst);
               ]))
      in
      match res with
      | List [] ->
          Int.equal (List.length lst) 0 || Int.equal (List.length lst) 1
      | List tail ->
          List.for_all2
            (fun a b -> Int64.equal a b)
            (List.hd lst :: List.map MLisp.Type.to_int tail)
            lst
      | _ -> false)

let test_cons =
  QCheck.Test.make ~count:100 ~name:"cons"
    QCheck.(pair int64 (small_list int64))
    (fun (i, lst) ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "cons";
                 Int i;
                 List (Symbol "list" :: List.map (fun n -> Type.Int n) lst);
               ]))
      in
      match res with
      | List l -> List.map MLisp.Type.to_int l = i :: lst
      | _ -> false)

let test_expt =
  QCheck.Test.make ~count:100 ~name:"expt ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "expt"; Int a; Int b ]))
      in
      match res with
      | Int n ->
          Int64.equal n (Int64.of_float (Int64.to_float a ** Int64.to_float b))
      | _ -> false)

let test_length =
  QCheck.Test.make ~count:100 ~name:"length"
    QCheck.(small_list int64)
    (fun lst ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "length";
                 List (Symbol "list" :: List.map (fun n -> Type.Int n) lst);
               ]))
      in
      match res with
      | Int len -> Int.equal (Int64.to_int len) (List.length lst)
      | _ -> false)

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

let test_max =
  QCheck.Test.make ~count:100 ~name:"max ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "max"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (max a b) | _ -> false)

let test_min =
  QCheck.Test.make ~count:100 ~name:"min ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "min"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (min a b) | _ -> false)

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

let test_round =
  QCheck.Test.make ~count:100 ~name:"round"
    QCheck.(pair int64 float)
    (fun (i, f) ->
      let int_res = MLisp.(eval global_env (List [ Symbol "round"; Int i ])) in
      let float_res =
        MLisp.(eval global_env (List [ Symbol "round"; Float f ]))
      in
      match (int_res, float_res) with
      | Int i1, Int i2 ->
          Int64.equal i i1
          && Int64.equal i2 (Int64.of_int (truncate (f +. 0.5)))
      | _ -> false)

let test_and =
  QCheck.Test.make ~count:100 ~name:"and"
    QCheck.(pair bool bool)
    (fun (b1, b2) ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "begin";
                 List [ Symbol "define"; Symbol "i"; Int 0L ];
                 List
                   [
                     Symbol "and";
                     Bool b1;
                     List
                       [
                         Symbol "begin";
                         List
                           [
                             Symbol "set!";
                             Symbol "i";
                             List [ Symbol "+"; Symbol "i"; Int 1L ];
                           ];
                         Bool b2;
                       ];
                   ];
                 Symbol "i";
               ]))
      in
      match res with
      | Int n ->
          let i = ref 0L in
          let _ =
            b1
            &&
            let () = i := Int64.add !i 1L in
            b2
          in
          Int64.equal n !i
      | _ -> false)

let test_or =
  QCheck.Test.make ~count:100 ~name:"or"
    QCheck.(pair bool bool)
    (fun (b1, b2) ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "begin";
                 List [ Symbol "define"; Symbol "i"; Int 0L ];
                 List
                   [
                     Symbol "or";
                     Bool b1;
                     List
                       [
                         Symbol "begin";
                         List
                           [
                             Symbol "set!";
                             Symbol "i";
                             List [ Symbol "+"; Symbol "i"; Int 1L ];
                           ];
                         Bool b2;
                       ];
                   ];
                 Symbol "i";
               ]))
      in
      match res with
      | Int n ->
          let i = ref 0L in
          let _ =
            b1
            ||
            let () = i := Int64.add !i 1L in
            b2
          in
          Int64.equal n !i
      | _ -> false)
