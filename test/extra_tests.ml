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
