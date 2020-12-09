let test_add =
  QCheck.Test.make ~count:100 ~name:"+ ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "+"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (Int64.add a b) | _ -> false)

let test_sub =
  QCheck.Test.make ~count:100 ~name:"- ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "-"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (Int64.sub a b) | _ -> false)

let test_mult =
  QCheck.Test.make ~count:100 ~name:"* ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "*"; Int a; Int b ])) in
      match res with Int n -> Int64.equal n (Int64.mul a b) | _ -> false)

let test_div =
  QCheck.Test.make ~count:100 ~name:"/ ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let () = QCheck.assume (not (Int64.equal b 0L)) in
      let res = MLisp.(eval global_env (List [ Symbol "/"; Int a; Int b ])) in
      match res with
      | Float n -> Float.equal n (Int64.to_float a /. Int64.to_float b)
      | _ -> false)

let test_gt =
  QCheck.Test.make ~count:100 ~name:"> ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol ">"; Int a; Int b ])) in
      match res with Bool r -> Bool.equal r (a > b) | _ -> false)

let test_lt =
  QCheck.Test.make ~count:100 ~name:"< ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "<"; Int a; Int b ])) in
      match res with Bool r -> Bool.equal r (a < b) | _ -> false)

let test_ge =
  QCheck.Test.make ~count:100 ~name:">= ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol ">="; Int a; Int b ])) in
      match res with Bool r -> Bool.equal r (a >= b) | _ -> false)

let test_le =
  QCheck.Test.make ~count:100 ~name:"<= ints"
    QCheck.(pair int64 int64)
    (fun (a, b) ->
      let res = MLisp.(eval global_env (List [ Symbol "<="; Int a; Int b ])) in
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
