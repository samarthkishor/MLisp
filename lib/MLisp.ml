(* MLisp
 * -----
 *
 * MIT License
 *
 * Copyright (c) 2020 Samarth Kishor
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

exception Not_implemented

exception Syntax_error of string

exception Runtime_error of string

module Type = struct
  (* TODO (#4) Implement support for floats *)
  type t =
    | Nil
    | Symbol of string
    | String of string
    | Int of Int64.t (* Python integers have arbitrary precision
                      * but this is the closest we can get in OCaml *)
    | Bool of bool
    | List of t list
    | Fun of (t list -> t)

  let rec to_string = function
    | Nil -> "nil"
    | Symbol s -> "'" ^ s (* Lisp symbols are often represented as 'symbol *)
    | String s -> s
    | Int n -> Int64.to_string n
    | Bool b -> string_of_bool b
    | List lst -> "[" ^ String.concat ", " (List.map to_string lst) ^ "]"
    | Fun _ -> "<fun>"

  let rec to_lispy = function
    | Nil -> "(quote ())"
    | Symbol s -> s
    | String s -> s
    | Int n -> Int64.to_string n
    | Bool b -> Bool.to_string b
    | List lst -> "(" ^ String.concat " " (List.map to_lispy lst) ^ ")"
    | Fun _ ->
        raise
          (Syntax_error
             "Fun should not be directly included in an MLisp program")

  let to_int = function
    | Int n -> n
    | _ -> raise (Invalid_argument "MLisp type error: expected Int")

  let to_bool = function
    | Bool b -> b
    | _ -> raise (Invalid_argument "MLisp type error: expected Bool")

  let rec equal a b =
    match (a, b) with
    | Nil, Nil -> true
    | Symbol a, Symbol b -> String.equal a b
    | String a, String b -> String.equal a b
    | Int a, Int b -> Int64.equal a b
    | Bool a, Bool b -> Bool.equal a b
    | List a, List b -> list_equal a b
    | _ -> false

  and list_equal a b =
    Int.equal (List.length a) (List.length b) && List.for_all2 equal a b

  let is_truthy = function Bool b -> b | Nil -> false | _ -> true
end

module Env = struct
  type t = { self : (string, Type.t) Hashtbl.t; outer : t option }

  let rec find key env =
    if Hashtbl.mem env.self key then Some env
    else match env.outer with None -> None | Some outer -> find key outer

  let get key env =
    match find key env with
    | None -> None
    | Some e -> Hashtbl.find_opt e.self key

  let put key value env = Hashtbl.replace env.self key value

  let init outer params_args =
    let self = Hashtbl.create 32 in
    let env = { self; outer } in
    let () = List.iter (fun (param, arg) -> put param arg env) params_args in
    env

  (* For debugging with #trace *)
  let pp ppf env =
    let rec pp_indent indent ppf env =
      let print_indent () =
        for _ = 0 to indent do
          Format.fprintf ppf "  "
        done
      in
      let () =
        print_indent ();
        Format.fprintf ppf "self env: ----------\n"
      in
      let () =
        Hashtbl.iter
          (fun key v ->
            print_indent ();
            Format.fprintf ppf "%s: %s\n" key (Type.to_string v))
          env.self
      in
      let () =
        print_indent ();
        Format.fprintf ppf "outer env: ---------\n"
      in
      let () =
        match env.outer with
        | None ->
            print_indent ();
            Format.fprintf ppf "None\n"
        | Some e -> pp_indent (indent + 1) ppf e
      in
      print_indent ();
      Format.fprintf ppf "--------------------\n"
    in
    pp_indent 0 ppf env

  (* For print debugging *)
  let print env = pp Format.std_formatter env
end

(* TODO (#1) Fix the bug which prevents lexical closures from working properly *)
(* TODO (#6) Implement `and` and `or` *)
let rec eval env = function
  | Type.Symbol s -> (
      match Env.get s env with
      | None -> raise (Runtime_error ("Symbol " ^ s ^ " not found in env"))
      | Some value -> value )
  | Type.List [ Type.Symbol "quote"; exp ] -> exp
  | Type.List [ Type.Symbol "if"; cond; then_branch; else_branch ] ->
      if Type.is_truthy (eval env cond) then eval env then_branch
      else eval env else_branch
  | Type.List [ Type.Symbol "define"; Symbol s; exp ] ->
      let () = Env.put s (eval env exp) env in
      Nil
  | Type.List [ Type.Symbol "set!"; Symbol s; exp ] ->
      let () = Env.put s (eval env exp) env in
      Nil
  | Type.List [ Type.Symbol "lambda"; Type.List params; body ] ->
      Type.Fun
        (fun args ->
          let evaled_params =
            List.map
              (fun param ->
                match param with
                | Type.Symbol s -> s
                | _ -> raise (Runtime_error "Lambda parameter must be a symbol"))
              params
          in
          let rec zip l1 l2 =
            match (l1, l2) with
            | [], [] -> []
            | _ :: _, [] | [], _ :: _ ->
                raise (Runtime_error "Lambda arguments do not match parameters")
            | x1 :: x1s, x2 :: x2s -> (x1, x2) :: zip x1s x2s
          in
          let sub_env = Env.init (Some env) (zip evaled_params args) in
          eval sub_env body)
  | Type.List (op :: args) -> (
      let proc = eval env op in
      match proc with
      | Type.Fun f -> f (List.map (fun arg -> eval env arg) args)
      | _ -> raise (Runtime_error "Cannot invoke non-function") )
  | ast -> ast

(* TODO (#2) Implement apply:
 * Takes two arguments. The first argument is a function and the second argument
 * is a list of the arguments to that function.
 *
 * e.g.
 *  List [Symbol "apply"; Symbol "+"; List [Symbol "list"; Int 1L; Int 2L]]
 *  is interpreted as
 *  List [Symbol "+"; Int 1L; Int 2L] *)
let apply_fun = raise Not_implemented

(* TODO (#2) Implement append:
 *   NOTE (lis.py's append function is Python's + operator
 *        which works for lists, strings, and numbers) *)
let append_fun = raise Not_implemented

(* TODO (#2) Implement car:
 *   NOTE raise an error if calling car on an empty list *)
let car_fun = raise Not_implemented

(* TODO (#2) Implement cdr:
 *   NOTE return an empty list if calling cdr on an empty list *)
let cdr_fun = raise Not_implemented

(* TODO (#2) Implement cons: *)
let cons_fun = raise Not_implemented

(* TODO (#2) Implement expt: *)
let expt_fun = raise Not_implemented

(* TODO (#2) Implement length: *)
let length_fun = raise Not_implemented

(* TODO (#2) Implement list?: *)
let is_list_fun = raise Not_implemented

(* TODO (#2) Implement map: *)
let map_fun = raise Not_implemented

(* TODO (#2) Implement max: *)
let max_fun = raise Not_implemented

(* TODO (#2) Implement min: *)
let min_fun = raise Not_implemented

let not_fun = function
  | [ Type.Bool b ] -> Type.Bool (not b)
  | _ -> raise (Runtime_error "Invalid argument(s): expected boolean")

(* TODO (#2) Implement null?: *)
let is_null_fun = raise Not_implemented

(* TODO (#2) Implement number?: *)
let is_number_fun = raise Not_implemented

(* TODO (#2) Implement procedure?  (hint: Type.Fun is the only type of procedure in MLisp): *)
let is_procedure_fun = raise Not_implemented

let is_symbol_fun = function
  | [ Type.Symbol _ ] -> Type.Bool true
  | _ -> Type.Bool false

(* TODO (#3a) Refactor add_fun, sub_fun, and div_fun into a single
 *            higher-order function *)
let add_fun = function
  | [ Type.Int a; Type.Int b ] -> Type.Int (Int64.add a b)
  | _ -> raise (Runtime_error "Invalid argument(s): expected 2 numbers")

let sub_fun = function
  | [ Type.Int a; Type.Int b ] -> Type.Int (Int64.sub a b)
  | _ -> raise (Runtime_error "Invalid argument(s): expected 2 numbers")

let mult_fun = function
  | [ Type.Int a; Type.Int b ] -> Type.Int (Int64.mul a b)
  | _ -> raise (Runtime_error "Invalid argument(s): expected 2 numbers")

let div_fun = function
  | [ Type.Int a; Type.Int b ] -> Type.Int (Int64.div a b)
  | _ -> raise (Runtime_error "Invalid argument(s): expected 2 numbers")

(* TODO (#3b) Refactor the following 4 functions into a single higher-order function *)
let gt_fun = function
  | [ Type.Int a; Type.Int b ] -> Type.Bool (a > b)
  | [ Type.String a; Type.String b ] -> Type.Bool (a > b)
  | _ ->
      raise
        (Runtime_error "Invalid argument(s): expected 2 numbers or 2 strings")

let lt_fun = function
  | [ Type.Int a; Type.Int b ] -> Type.Bool (a < b)
  | [ Type.String a; Type.String b ] -> Type.Bool (a < b)
  | _ ->
      raise
        (Runtime_error "Invalid argument(s): expected 2 numbers or 2 strings")

let ge_fun = function
  | [ Type.Int a; Type.Int b ] -> Type.Bool (a >= b)
  | [ Type.String a; Type.String b ] -> Type.Bool (a >= b)
  | _ ->
      raise
        (Runtime_error "Invalid argument(s): expected 2 numbers or 2 strings")

let le_fun = function
  | [ Type.Int a; Type.Int b ] -> Type.Bool (a <= b)
  | [ Type.String a; Type.String b ] -> Type.Bool (a <= b)
  | _ ->
      raise
        (Runtime_error "Invalid argument(s): expected 2 numbers or 2 strings")

(* end (#3) *)

let equal_fun = function
  | [ a; b ] -> Type.Bool (Type.equal a b)
  | _ -> raise (Runtime_error "Invalid argument(s): expected 2 expressions")

let eq_fun = function
  | [ a; b ] -> Type.Bool (a == b) (* ( == ) is reference equality *)
  | _ -> raise (Runtime_error "Invalid argument(s): expected 2 expressions")

let print_fun = function
  | [ exp ] ->
      let () = print_endline (Type.to_string exp) in
      Type.Nil
  | _ -> raise (Runtime_error "Invalid argument(s): expected 1 expression")

(* TODO (#5) Implment round: *)
let round_fun = raise Not_implemented

(* global environment *)
let global_env =
  Env.init None
    [
      ("+", Type.Fun add_fun);
      ("-", Type.Fun sub_fun);
      ("*", Type.Fun mult_fun);
      ("/", Type.Fun div_fun);
      ("=", Type.Fun equal_fun);
      (">", Type.Fun gt_fun);
      ("<", Type.Fun lt_fun);
      (">=", Type.Fun ge_fun);
      ("<=", Type.Fun le_fun);
      ("append", Type.Fun append_fun);
      ("apply", Type.Fun apply_fun);
      ("begin", Type.Fun (fun args -> List.hd (List.rev args)));
      ("car", Type.Fun car_fun);
      ("cdr", Type.Fun cdr_fun);
      ("cons", Type.Fun cons_fun);
      ("eq?", Type.Fun eq_fun);
      ("expt", Type.Fun expt_fun);
      ("equal?", Type.Fun equal_fun);
      ("length", Type.Fun length_fun);
      ("list", Type.Fun (fun args -> List args));
      ("list?", Type.Fun is_list_fun);
      ("map", Type.Fun map_fun);
      ("max", Type.Fun max_fun);
      ("min", Type.Fun min_fun);
      ("not", Type.Fun not_fun);
      ("null?", Type.Fun is_null_fun);
      ("number?", Type.Fun is_number_fun);
      ("print", Type.Fun print_fun);
      ("procedure?", Type.Fun is_procedure_fun);
      ("symbol?", Type.Fun is_symbol_fun);
      ("round", Type.Fun round_fun);
    ]
