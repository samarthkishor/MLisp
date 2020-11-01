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
  QCheck.Test.make ~count:100 ~name:"car"
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
  QCheck.Test.make ~count:100 ~name:"expt"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "expt"; Int a; Int b ]))
      in
      match res with
      | Int n ->
          Int64.equal n (Int64.of_float (Int64.to_float a ** Int64.to_float b))
      | _ -> false)

let test_expt_float_1 =
  QCheck.Test.make ~count:100 ~name:"expt floats (1)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "expt"; Int a; Float b ]))
      in
      match res with
      | Float n -> Float.equal n (Int64.to_float a ** b)
      | _ -> false)

let test_expt_float_2 =
  QCheck.Test.make ~count:100 ~name:"expt floats (2)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "expt"; Float a; Int b ]))
      in
      match res with
      | Float n -> Float.equal n (a ** Int64.to_float b)
      | _ -> false)

let test_expt_float_3 =
  QCheck.Test.make ~count:100 ~name:"expt floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "expt"; Float a; Float b ]))
      in
      match res with Float n -> Float.equal n (a ** b) | _ -> false)

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

let test_max =
  QCheck.Test.make ~count:100 ~name:"max"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "max"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (max a b) | _ -> false)

let test_max_float_1 =
  QCheck.Test.make ~count:100 ~name:"max floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "max"; Float a; Int b ]))
      in
      match res with
      | Float n -> Float.equal n (max a (Int64.to_float b))
      | _ -> false)

let test_max_float_2 =
  QCheck.Test.make ~count:100 ~name:"max floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "max"; Int a; Float b ]))
      in
      match res with
      | Float n -> Float.equal n (max b (Int64.to_float a))
      | _ -> false)

let test_max_float_3 =
  QCheck.Test.make ~count:100 ~name:"max floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "max"; Float a; Float b ]))
      in
      match res with Float n -> Float.equal n (max a b) | _ -> false)

let test_min =
  QCheck.Test.make ~count:100 ~name:"min"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "min"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (min a b) | _ -> false)

let test_min_float_1 =
  QCheck.Test.make ~count:100 ~name:"min floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "min"; Float a; Int b ]))
      in
      match res with
      | Float n -> Float.equal n (min a (Int64.to_float b))
      | _ -> false)

let test_min_float_2 =
  QCheck.Test.make ~count:100 ~name:"min floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "min"; Int a; Float b ]))
      in
      match res with
      | Float n -> Float.equal n (min b (Int64.to_float a))
      | _ -> false)

let test_min_float_3 =
  QCheck.Test.make ~count:100 ~name:"min floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "min"; Float a; Float b ]))
      in
      match res with Float n -> Float.equal n (min a b) | _ -> false)

let test_add =
  QCheck.Test.make ~count:100 ~name:"+"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "+"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (Int64.add a b) | _ -> false)

let test_add_float_1 =
  QCheck.Test.make ~count:100 ~name:"+ floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "+"; Float a; Int b ])) in
      match res with
      | Float n -> Float.equal n (a +. Int64.to_float b)
      | _ -> false)

let test_add_float_2 =
  QCheck.Test.make ~count:100 ~name:"+ floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "+"; Int a; Float b ])) in
      match res with
      | Float n -> Float.equal n (Int64.to_float a +. b)
      | _ -> false)

let test_add_float_3 =
  QCheck.Test.make ~count:100 ~name:"+ floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "+"; Float a; Float b ]))
      in
      match res with Float n -> Float.equal n (a +. b) | _ -> false)

let test_sub =
  QCheck.Test.make ~count:100 ~name:"-"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "-"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (Int64.sub a b) | _ -> false)

let test_sub_float_1 =
  QCheck.Test.make ~count:100 ~name:"- floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "-"; Float a; Int b ])) in
      match res with
      | Float n -> Float.equal n (a -. Int64.to_float b)
      | _ -> false)

let test_sub_float_2 =
  QCheck.Test.make ~count:100 ~name:"- floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "-"; Int a; Float b ])) in
      match res with
      | Float n -> Float.equal n (Int64.to_float a -. b)
      | _ -> false)

let test_sub_float_3 =
  QCheck.Test.make ~count:100 ~name:"- floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "-"; Float a; Float b ]))
      in
      match res with Float n -> Float.equal n (a -. b) | _ -> false)

let test_mult =
  QCheck.Test.make ~count:100 ~name:"*"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "*"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (Int64.mul a b) | _ -> false)

let test_mult_float_1 =
  QCheck.Test.make ~count:100 ~name:"* floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "*"; Float a; Int b ])) in
      match res with
      | Float n -> Float.equal n (a *. Int64.to_float b)
      | _ -> false)

let test_mult_float_2 =
  QCheck.Test.make ~count:100 ~name:"* floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "*"; Int a; Float b ])) in
      match res with
      | Float n -> Float.equal n (Int64.to_float a *. b)
      | _ -> false)

let test_mult_float_3 =
  QCheck.Test.make ~count:100 ~name:"* floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "*"; Float a; Float b ]))
      in
      match res with Float n -> Float.equal n (a *. b) | _ -> false)

let test_div =
  QCheck.Test.make ~count:100 ~name:"/"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let () = QCheck.assume (not (Int64.equal b 0L)) in
      let res = MLisp.(eval global_env (List [ Symbol "/"; Int a; Int b ])) in
      match res with
      | Float n -> Float.equal n (Int64.to_float a /. Int64.to_float b)
      | _ -> false)

let test_div_float_1 =
  QCheck.Test.make ~count:100 ~name:"/ floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "/"; Float a; Int b ])) in
      match res with
      | Float n -> Float.equal n (a /. Int64.to_float b)
      | _ -> false)

let test_div_float_2 =
  QCheck.Test.make ~count:100 ~name:"/ floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "/"; Int a; Float b ])) in
      match res with
      | Float n -> Float.equal n (Int64.to_float a /. b)
      | _ -> false)

let test_div_float_3 =
  QCheck.Test.make ~count:100 ~name:"/ floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "/"; Float a; Float b ]))
      in
      match res with Float n -> Float.equal n (a /. b) | _ -> false)

let test_gt =
  QCheck.Test.make ~count:100 ~name:">"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol ">"; Int a; Int b ])) in
      match res with Bool r -> Bool.equal r (a > b) | _ -> false)

let test_gt_float_1 =
  QCheck.Test.make ~count:100 ~name:"> floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol ">"; Float a; Int b ])) in
      match res with
      | Bool r -> Bool.equal r (a > Int64.to_float b)
      | _ -> false)

let test_gt_float_2 =
  QCheck.Test.make ~count:100 ~name:"> floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol ">"; Int a; Float b ])) in
      match res with
      | Bool r -> Bool.equal r (Int64.to_float a > b)
      | _ -> false)

let test_gt_float_3 =
  QCheck.Test.make ~count:100 ~name:"> floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol ">"; Float a; Float b ]))
      in
      match res with Bool r -> Bool.equal r (a > b) | _ -> false)

let test_lt =
  QCheck.Test.make ~count:100 ~name:"<"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "<"; Int a; Int b ])) in
      match res with Bool r -> Bool.equal r (a < b) | _ -> false)

let test_lt_float_1 =
  QCheck.Test.make ~count:100 ~name:"< floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "<"; Float a; Int b ])) in
      match res with
      | Bool r -> Bool.equal r (a < Int64.to_float b)
      | _ -> false)

let test_lt_float_2 =
  QCheck.Test.make ~count:100 ~name:"< floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "<"; Int a; Float b ])) in
      match res with
      | Bool r -> Bool.equal r (Int64.to_float a < b)
      | _ -> false)

let test_lt_float_3 =
  QCheck.Test.make ~count:100 ~name:"< floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "<"; Float a; Float b ]))
      in
      match res with Bool r -> Bool.equal r (a < b) | _ -> false)

let test_ge =
  QCheck.Test.make ~count:100 ~name:">="
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol ">="; Int a; Int b ])) in
      match res with Bool r -> Bool.equal r (a >= b) | _ -> false)

let test_ge_float_1 =
  QCheck.Test.make ~count:100 ~name:">= floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol ">="; Float a; Int b ]))
      in
      match res with
      | Bool r -> Bool.equal r (a >= Int64.to_float b)
      | _ -> false)

let test_ge_float_2 =
  QCheck.Test.make ~count:100 ~name:">= floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol ">="; Int a; Float b ]))
      in
      match res with
      | Bool r -> Bool.equal r (Int64.to_float a >= b)
      | _ -> false)

let test_ge_float_3 =
  QCheck.Test.make ~count:100 ~name:">= floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol ">="; Float a; Float b ]))
      in
      match res with Bool r -> Bool.equal r (a >= b) | _ -> false)

let test_le =
  QCheck.Test.make ~count:100 ~name:"<="
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "<="; Int a; Int b ])) in
      match res with Bool r -> Bool.equal r (a <= b) | _ -> false)

let test_le_float_1 =
  QCheck.Test.make ~count:100 ~name:"<= floats (1)"
    QCheck.(pair float int64)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "<="; Float a; Int b ]))
      in
      match res with
      | Bool r -> Bool.equal r (a <= Int64.to_float b)
      | _ -> false)

let test_le_float_2 =
  QCheck.Test.make ~count:100 ~name:"<= floats (2)"
    QCheck.(pair int64 float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "<="; Int a; Float b ]))
      in
      match res with
      | Bool r -> Bool.equal r (Int64.to_float a <= b)
      | _ -> false)

let test_le_float_3 =
  QCheck.Test.make ~count:100 ~name:"<= floats (3)"
    QCheck.(pair float float)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "<="; Float a; Float b ]))
      in
      match res with Bool r -> Bool.equal r (a <= b) | _ -> false)

let test_gt_string =
  QCheck.Test.make ~count:100 ~name:"> strings"
    QCheck.(pair printable_string printable_string)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol ">"; String a; String b ]))
      in
      match res with Bool r -> Bool.equal r (a > b) | _ -> false)

let test_lt_string =
  QCheck.Test.make ~count:100 ~name:"< strings"
    QCheck.(pair printable_string printable_string)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "<"; String a; String b ]))
      in
      match res with Bool r -> Bool.equal r (a < b) | _ -> false)

let test_ge_string =
  QCheck.Test.make ~count:100 ~name:">= strings"
    QCheck.(pair printable_string printable_string)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol ">="; String a; String b ]))
      in
      match res with Bool r -> Bool.equal r (a >= b) | _ -> false)

let test_le_string =
  QCheck.Test.make ~count:100 ~name:"<= strings"
    QCheck.(pair printable_string printable_string)
    (fun (a, b) ->
      let res =
        MLisp.(eval global_env (List [ Symbol "<="; String a; String b ]))
      in
      match res with Bool r -> Bool.equal r (a <= b) | _ -> false)

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

(* Full programs *)

let test_x_times_x =
  QCheck.Test.make ~count:100 ~name:"(begin (define x _) ( * x x))"
    QCheck.(int64)
    (fun x ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "begin";
                 List [ Symbol "define"; Symbol "x"; Int x ];
                 List [ Symbol "*"; Symbol "x"; Symbol "x" ];
               ]))
      in
      match res with Int n -> Int64.equal n (Int64.mul x x) | _ -> false)

let test_apply =
  QCheck.Test.make ~count:100 ~name:"(apply + (list 1 2))"
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

let test_lambda =
  QCheck.Test.make ~count:100
    ~name:"(begin (define x \"_\") ((lambda (x) (+ x 1)) _))"
    QCheck.(pair string int64)
    (fun (str, i) ->
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "begin";
                 List [ Symbol "define"; Symbol "x"; String str ];
                 List
                   [
                     List
                       [
                         Symbol "lambda";
                         List [ Symbol "x" ];
                         List [ Symbol "+"; Symbol "x"; Int 1L ];
                       ];
                     Int i;
                   ];
               ]))
      in
      match res with Int n -> Int64.equal n (Int64.add i 1L) | _ -> false)

let test_recursion =
  QCheck.Test.make ~count:10
    ~name:
      "(begin (define fact (lambda (n) (if (<= n 1) 1 ( * n (fact (- n 1)))))) \
       (fact 10))" QCheck.int64 (fun i ->
      let () = QCheck.assume (i < 10L) in
      let res =
        MLisp.(
          eval global_env
            (List
               [
                 Symbol "begin";
                 List
                   [
                     Symbol "define";
                     Symbol "fact";
                     List
                       [
                         Symbol "lambda";
                         List [ Symbol "n" ];
                         List
                           [
                             Symbol "if";
                             List [ Symbol "<="; Symbol "n"; Int 1L ];
                             Int 1L;
                             List
                               [
                                 Symbol "*";
                                 Symbol "n";
                                 List
                                   [
                                     Symbol "fact";
                                     List [ Symbol "-"; Symbol "n"; Int 1L ];
                                   ];
                               ];
                           ];
                       ];
                   ];
                 List [ Symbol "fact"; Int i ];
               ]))
      in
      match res with
      | Int n ->
          let rec fact n =
            if n <= 1L then 1L else Int64.mul n (fact (Int64.sub n 1L))
          in
          Int64.equal n (fact i)
      | _ -> false)

let test_and =
  QCheck.Test.make ~count:100
    ~name:"(begin (define i 0) (and _ (begin (set! i (+ i 1)) _)) i)"
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
  QCheck.Test.make ~count:100
    ~name:"(begin (define i 0) (or _ (begin (set! i (+ i 1)) _)) i)"
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

let () =
  let open Alcotest in
  let added_funs_generated_tests =
    List.map QCheck_alcotest.to_alcotest
      [
        test_append_lists;
        test_append_strings;
        test_append_numbers;
        test_car;
        test_cdr;
        test_cons;
        test_expt;
        test_expt_float_1;
        test_expt_float_2;
        test_expt_float_3;
        test_length;
        test_max;
        test_max_float_1;
        test_max_float_2;
        test_max_float_3;
        test_min;
        test_min_float_1;
        test_min_float_2;
        test_min_float_3;
        test_add;
        test_add_float_1;
        test_add_float_2;
        test_add_float_3;
        test_sub;
        test_sub_float_1;
        test_sub_float_2;
        test_sub_float_3;
        test_mult;
        test_mult_float_1;
        test_mult_float_2;
        test_mult_float_3;
        test_div;
        test_div_float_1;
        test_div_float_2;
        test_div_float_3;
        test_gt;
        test_gt_float_1;
        test_gt_float_2;
        test_gt_float_3;
        test_lt;
        test_lt_float_1;
        test_lt_float_2;
        test_lt_float_3;
        test_ge;
        test_ge_float_1;
        test_ge_float_2;
        test_ge_float_3;
        test_le;
        test_le_float_1;
        test_le_float_2;
        test_le_float_3;
        test_gt_string;
        test_lt_string;
        test_ge_string;
        test_le_string;
        test_round;
      ]
  in
  let mlisp_program_generated_tests =
    List.map QCheck_alcotest.to_alcotest
      [
        test_x_times_x;
        test_apply;
        test_lambda;
        test_recursion;
        test_and;
        test_or;
      ]
  in
  run "MLisp randomly generated tests"
    [
      ("Core functions", added_funs_generated_tests);
      ("MLisp programs", mlisp_program_generated_tests);
    ]
