# MLisp

## Introduction

MLisp is based off of Peter Norvig's lis.py (https://www.norvig.com/lispy.html).

MLisp is an incomplete lis.py implementation written in OCaml intended for
educational purposes. The goal of MLisp is to teach the concept of "code is
data", so there is no s-expression parser. You construct MLisp programs
using OCaml datatypes, meaning that MLisp is a Lisp interpreter embedded in
OCaml. You can extend the functionality of MLisp by adding features
to the core implementation.

## Differences from lis.py:

- MLisp has explicit string, boolean, and symbol types.
- lis.py has special math functions like sin, cos, etc while MLisp does not.
- MLisp has 64-bit ints and floats while lis.py has arbitrary-precision
  numbers.
- MLisp has short-circuiting boolean `and` and `or` functions.

## Hints:

1. If you get stuck, read the Python source code and try to translate it into
   OCaml. There will likely be some major differences due to OCaml's strict
   static type system, but the core implementation of MLisp is similar to
   lis.py. Since MLisp is an implementation of lis.py, a MLisp program should
   have the same output as the equivalent lis.py program.

2. Debugging hints:

   1. Use `let () = Env.print env in ...` to print the environment

   2. Trace functions to see what they're returning.
      This only works locally.
      Use `#trace` in the OCaml toplevel (preferably utop) via:

   ```
    utop[1]> #install_printer Env.pp;;
    utop[2]> #trace Env.get;;
    Env.get is now traced.
    utop[3]> eval global_env (List [Symbol "+"; Int 1L; Int 2L]);;
    Env.get <-- "+"
    Env.get --> <fun>
    Env.get* <--
    self env: ----------
    *: <fun>
    +: <fun>
    [ ... ]
    symbol?: <fun>
    outer env: ---------
    None

    Env.get\* --> Some (Type.Fun <fun>)

    - : Type.t = Type.Int 3L
   ```

## Instructions:

1. Fix the bug which prevents lexical closures from working properly

2. Implement the remaining lis.py functions:

   - `apply`
   - `append`
   - `car`
   - `cdr`
   - `cons`
   - `expt`
   - `length`
   - `list?`
   - `map`
   - `max`
   - `min`
   - `null?`
   - `number?`
   - `procedure?` (hint: `Type.Fun` is the only type of procedure in MLisp)

3. Refactor the following into a single higher-order function:

   a. `add_fun`, `sub_fun`, and `mult_fun`

   b. `gt_fun`, `lt_fun`, `ge_fun`, and `le_fun`

4. Implement support for floats.
   Hint: you may need to extend Type.t with a `Float of float` type and
   add float support to all the functions that work with `Int`s.
   OCaml floats are 64 bit so don't worry about overflow.
   Note that division in lis.py always returns a float

5. Implement the `round` function

6. Implement short-circuiting boolean `and` and `or` forms in `eval`.
   To make things easier, assume that they only take two arguments.
   Hint: These are sometimes defined as macros in others Lisps.
   Look at the macros and see how you can transform `and` and `or` into `if` expressions

## Usage:

Run `make` to install the dependencies and `make build` to build the project.

You should be using OCaml 4.10.0 or later.

`make test` runs the test suite and `make coverage` generates a HTML test coverage report at `_coverage/index.html`.

`make fmt` auto-formats all the OCaml files with `ocamlformat`.

`make repl` runs a custom instance of `utop` with the `MLisp` library pre-loaded.
You will have to re-run this command every time you change `MLisp.ml`.

The MLisp code is at `lib/MLisp.ml` and has no extra dependencies so you can
copy-paste it into an online editor like https://try.ocamlpro.com/ and it will work.

## Sample MLisp Programs:

### Multiplication with variables

#### lis.py code

```scheme
(begin
  (define x 5)
  ( * x x))
```

#### MLisp code

```ocaml
eval global_env (List [Symbol "begin"; List [Symbol "define"; Symbol "x"; Int 5L]; List [Symbol "*"; Symbol "x"; Symbol "x"]]);;
```

Output:

```
- : Type.t = Type.Int 25L
```

### Apply

Note that MLisp follows lis.py in this case...

`(apply + (list 1 2 3))` or `(+ 1 2 3)` would evaluate to 1 + 2 + 3 in most Lisps

#### lis.py code

```scheme
(apply + (list 1 2))
```

### MLisp code

```ocaml
eval global_env (List [Symbol "apply"; Symbol "+"; List [Symbol "list"; Int 1L; Int 2L]]);;
```

Output:

```
- : Type.t = Type.Int 3
```

### Apply Error

#### lis.py code

```scheme
(apply + (list 1 2 3))
```

#### MLisp code

```ocaml
eval global_env (List [Symbol "apply"; Symbol "+"; List [Symbol "list"; Int 1L; Int 2L; Int 3L]]);;
```

Output:

```
Exception: Runtime_error "Invalid argument(s): expected 2 numbers".
```

### Lambdas

#### lis.py code

```scheme
(begin
  (define x "test")
  ((lambda (x) (+ x 1)) 2))
```

#### MLisp code

```ocaml
eval global_env
(List [Symbol "begin";
   List [Symbol "define"; Symbol "x"; String "test"];
   List [List [Symbol "lambda"; List [Symbol "x"];
     List [Symbol "+"; Symbol "x"; Int 1L]];
   Int 2L]]);;
```

Output:

```
- : Type.t = Type.Int 3L
```

### Recursion

#### lis.py code

```scheme
(begin
  (define fact (lambda (n) (if (<= n 1) 1 ( _ n (fact (- n 1))))))
  (fact 10))
```

#### MLisp code

```ocaml
eval global_env
(List
  [Symbol "begin";
    List [Symbol "define"; Symbol "fact";
      List [Symbol "lambda"; List [Symbol "n"];
        List [Symbol "if"; List [Symbol "<="; Symbol "n"; Int 1L];
          Int 1L;
          List [Symbol "*"; Symbol "n"; List [Symbol "fact"; List [Symbol "-"; Symbol "n"; Int 1L]]]]]];
    List [Symbol "fact"; Int 10L]]);;
```

Output:

```
- : Type.t = Type.Int 3628800L
```

### Lexical Closures

#### lis.py code

```scheme
(begin
 (define make-counter
   (lambda ()
     (begin
      (define count 0)
      (lambda ()
        (begin
         (set! count (+ count 1))
         count)))))
 (begin
  (define counter1 (make-counter))
  (define counter2 (make-counter))
  (print (counter1))
  (print (counter1))
  (print (counter2))
  (print (counter2))))
```

#### MLisp code

```ocaml
eval global_env
  (List
     [Symbol "begin";
      List [Symbol "define"; Symbol "make-counter";
            List [Symbol "lambda"; List [];
                  List [Symbol "begin";
                        List [Symbol "define"; Symbol "count"; Int 0L];
                        List [Symbol "lambda"; List [];
                              List [Symbol "begin";
                                    List [Symbol "set!"; Symbol "count";
                                          List [Symbol "+"; Symbol "count"; Int 1L]];
                                    Symbol "count"]]]]];
      List [Symbol "begin";
            List [Symbol "define"; Symbol "counter1"; List [Symbol "make-counter"]];
            List [Symbol "print"; List [Symbol "counter1"]];
            List [Symbol "print"; List [Symbol "counter1"]];
           ]]);;
```

Output:

```
1
2
- : Type.t = Type.Nil
```

### Short-circuiting `and`

When the first argument is "falsy":

#### lis.py code

```scheme
(and
  (begin (print "left") #f) ;; #f is false and #t is true
  (begin (print "right") #t))
```

#### MLisp code

```ocaml
eval global_env
  (List [ Symbol "and";
    List [ Symbol "begin"; List [ Symbol "print"; String "left" ]; Bool false];
    List [ Symbol "begin"; List [ Symbol "print"; String "right" ]; Bool true]]);;
```

Output:

```
left
- : Type.t = Type.Bool false
```

When the first argument is "falsy":

#### lis.py code

```scheme
(and
  (begin (print "left") #t) ;; #f is false and #t is true
  (begin (print "right") #t))
```

#### MLisp code

```ocaml
eval global_env
  (List [ Symbol "and";
    List [ Symbol "begin"; List [ Symbol "print"; String "left" ]; Bool true];
    List [ Symbol "begin"; List [ Symbol "print"; String "right" ]; Bool true]]);;
```

Output:

```
left
right
- : Type.t = Type.Bool true
```

### Short-circuiting `or`

When the first argument is "truthy":

#### lis.py code

```scheme
(or
  (begin (print "left") #t) ;; #f is false and #t is true
  (begin (print "right") #f))
```

#### MLisp code

```ocaml
eval global_env
  (List [ Symbol "or";
    List [ Symbol "begin"; List [ Symbol "print"; String "left" ]; Bool true];
    List [ Symbol "begin"; List [ Symbol "print"; String "right" ]; Bool false]]);;
```

Output:

```
left
- : Type.t = Type.Bool true
```

When the first argument is "falsy":

#### lis.py code

```scheme
(or
  (begin (print "left") #f) ;; #f is false and #t is true
  (begin (print "right") #f))
```

#### MLisp code

```ocaml
eval global_env
  (List [ Symbol "or";
    List [ Symbol "begin"; List [ Symbol "print"; String "left" ]; Bool false];
    List [ Symbol "begin"; List [ Symbol "print"; String "right" ]; Bool false]]);;
```

Output:

```
left
right
- : Type.t = Type.Bool false
```
