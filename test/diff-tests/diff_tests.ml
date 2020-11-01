let lispy_eval program =
  let f = Py.Module.get_function (Py.import "lis") "eval_program" in
  Py.String.to_string
    (f (Array.map Py.String.of_string [| MLisp.Type.to_lispy program |]))

let leaf n = MLisp.Type.Int n

let binop op x y = MLisp.(Type.List [ Symbol op; x; y ])

let gen_binop op_array =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
           let op = op_array.(n mod Array.length op_array) in
           match n with
           | 0 -> map leaf ui64
           | n ->
               frequency
                 [
                   (1, map leaf ui64);
                   (2, map2 (binop op) (self (n / 2)) (self (n / 2)));
                 ]))

let arbitrary_binop_program generator =
  let open QCheck.Iter in
  let open MLisp in
  let rec shrink_tree = function
    | Type.Int i -> QCheck.Shrink.int64 i >|= leaf
    | List [ Symbol op; a; b ] ->
        of_list [ a; b ]
        <+> (shrink_tree a >|= fun a' -> binop op a' b)
        <+> (shrink_tree b >|= fun b' -> binop op a b')
    | _ -> fun _ -> ()
  in
  QCheck.make generator ~print:Type.to_lispy ~shrink:shrink_tree

let test_arithmetic =
  (* does not test division due to division by zero errors *)
  QCheck.Test.make ~name:"arithmetic programs" ~count:100
    (arbitrary_binop_program (gen_binop [| "+"; "-"; "*" |]))
    (fun program ->
      try
        let mlisp_res = MLisp.(eval global_env program |> Type.to_lispy) in
        let lispy_res = lispy_eval program in
        String.equal mlisp_res lispy_res
      with Division_by_zero -> true)

let test_minmax =
  QCheck.Test.make ~name:"min/max programs" ~count:100
    (arbitrary_binop_program (gen_binop [| "max"; "min" |]))
    (fun program ->
      let mlisp_res = MLisp.(eval global_env program |> Type.to_lispy) in
      let lispy_res = lispy_eval program in
      String.equal mlisp_res lispy_res)

let () =
  Py.initialize ();
  let open Alcotest in
  let mlisp_program_differential_tests =
    List.map QCheck_alcotest.to_alcotest [ test_arithmetic; test_minmax ]
  in
  run "MLisp differential fuzz tests"
    [ ("random programs", mlisp_program_differential_tests) ]
