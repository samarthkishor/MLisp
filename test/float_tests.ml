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
