(* In this file we test the computation of the set of last used variables and the set of to-be-removed unused variables for different node types and cases *)

open Core
open Lltz_ir
open Ast_builder.With_dummy
module LV = Lltz_codegen.Last_vars
module Michelson_base = Smartpy_michelson_base

let rec print_annotations ?(indent = 0) (e : Expr.t) =
  let pad = String.make indent ' ' in
  let lu_str = Set.to_list e.annotations.last_used_vars |> String.concat ~sep:", " in
  let rnu_str =
    Set.to_list e.annotations.remove_never_used_vars |> String.concat ~sep:", "
  in
  let print_intro label =
    Printf.printf "%sNode: %s\n" pad label;
    Printf.printf "%s  luv = {%s} nuv = {%s} \n" pad lu_str rnu_str
    (*luv - last_used_vars, nuv - never_used_vars*)
  in
  match e.desc with
  | Expr.Let_in { let_var = Var x; rhs; in_ } ->
    print_intro "Let_in";
    Printf.printf "%s  let_var=%s\n" pad x;
    Printf.printf "%s  rhs:\n" pad;
    print_annotations ~indent:(indent + 2) rhs;
    Printf.printf "%s  in_:\n" pad;
    print_annotations ~indent:(indent + 2) in_
  | Expr.Let_mut_in { let_var = Mut_var x; rhs; in_ } ->
    print_intro "Let_mut_in";
    Printf.printf "%s  let_var=%s\n" pad x;
    Printf.printf "%s  rhs:\n" pad;
    print_annotations ~indent:(indent + 2) rhs;
    Printf.printf "%s  in_:\n" pad;
    print_annotations ~indent:(indent + 2) in_
  | Expr.Lambda_rec
      { mu_var = Var mu, _mu_ty; lambda = { lam_var = Var arg, _arg_ty; body } } ->
    print_intro "Lambda_rec";
    Printf.printf "%s  mu_var=%s\n" pad mu;
    Printf.printf "%s  lam_var=%s\n" pad arg;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.Lambda { lam_var = Var arg, _arg_ty; body } ->
    print_intro "Lambda";
    Printf.printf "%s  lam_var=%s\n" pad arg;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.App { abs; arg } ->
    print_intro "App";
    Printf.printf "%s  abs:\n" pad;
    print_annotations ~indent:(indent + 2) abs;
    Printf.printf "%s  arg:\n" pad;
    print_annotations ~indent:(indent + 2) arg
  | Expr.Const _ -> print_intro "Const"
  | Expr.Prim (_, args) ->
    print_intro "Prim";
    Printf.printf "%s  args:\n" pad;
    List.iter ~f:(fun x -> print_annotations ~indent:(indent + 2) x) args
  | Expr.Assign (Mut_var x, v) ->
    print_intro "Assign";
    Printf.printf "%s  let_var=%s\n" pad x;
    Printf.printf "%s  value:\n" pad;
    print_annotations ~indent:(indent + 2) v
  | Expr.If_bool { condition; if_true; if_false } ->
    print_intro "If_bool";
    Printf.printf "%s  condition:\n" pad;
    print_annotations ~indent:(indent + 2) condition;
    Printf.printf "%s  if_true:\n" pad;
    print_annotations ~indent:(indent + 2) if_true;
    Printf.printf "%s  if_false:\n" pad;
    print_annotations ~indent:(indent + 2) if_false
  | Expr.If_none { subject; if_none; if_some = { lam_var = Var x, _ty; body } } ->
    print_intro "If_none";
    Printf.printf "%s  subject:\n" pad;
    print_annotations ~indent:(indent + 2) subject;
    Printf.printf "%s  if_none:\n" pad;
    print_annotations ~indent:(indent + 2) if_none;
    Printf.printf "%s  if_some lam_var=%s\n" pad x;
    Printf.printf "%s  if_some body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.If_cons
      { subject
      ; if_empty
      ; if_nonempty = { lam_var1 = Var hd, _ty1; lam_var2 = Var tl, _ty2; body }
      } ->
    print_intro "If_cons";
    Printf.printf "%s  subject:\n" pad;
    print_annotations ~indent:(indent + 2) subject;
    Printf.printf "%s  if_empty:\n" pad;
    print_annotations ~indent:(indent + 2) if_empty;
    Printf.printf "%s  if_nonempty lam_var1=%s lam_var2=%s\n" pad hd tl;
    Printf.printf "%s  if_nonempty body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.If_left
      { subject
      ; if_left = { lam_var = Var xl, _tyl; body = bl }
      ; if_right = { lam_var = Var xr, _tyr; body = br }
      } ->
    print_intro "If_left";
    Printf.printf "%s  subject:\n" pad;
    print_annotations ~indent:(indent + 2) subject;
    Printf.printf "%s  if_left lam_var=%s\n" pad xl;
    print_annotations ~indent:(indent + 2) bl;
    Printf.printf "%s  if_right lam_var=%s\n" pad xr;
    print_annotations ~indent:(indent + 2) br
  | Expr.While { cond; body } ->
    print_intro "While";
    Printf.printf "%s  cond:\n" pad;
    print_annotations ~indent:(indent + 2) cond;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.While_left { cond; body = { lam_var = Var x, _ty; body } } ->
    print_intro "While_left";
    Printf.printf "%s  cond:\n" pad;
    print_annotations ~indent:(indent + 2) cond;
    Printf.printf "%s  lam_var=%s\n" pad x;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.For { index = Mut_var idx; init; cond; update; body } ->
    print_intro "For";
    Printf.printf "%s  index=%s\n" pad idx;
    Printf.printf "%s  init:\n" pad;
    print_annotations ~indent:(indent + 2) init;
    Printf.printf "%s  cond:\n" pad;
    print_annotations ~indent:(indent + 2) cond;
    Printf.printf "%s  update:\n" pad;
    print_annotations ~indent:(indent + 2) update;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.For_each { collection; body = { lam_var = Var x, _ty; body } } ->
    print_intro "For_each";
    Printf.printf "%s  collection:\n" pad;
    print_annotations ~indent:(indent + 2) collection;
    Printf.printf "%s  lam_var=%s\n" pad x;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.Map { collection; map = { lam_var = Var x, _ty; body } } ->
    print_intro "Map";
    Printf.printf "%s  collection:\n" pad;
    print_annotations ~indent:(indent + 2) collection;
    Printf.printf "%s  lam_var=%s\n" pad x;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.Fold_left { collection; init; fold = { lam_var = Var x, _ty; body } } ->
    print_intro "Fold_left";
    Printf.printf "%s  collection:\n" pad;
    print_annotations ~indent:(indent + 2) collection;
    Printf.printf "%s  init:\n" pad;
    print_annotations ~indent:(indent + 2) init;
    Printf.printf "%s  lam_var=%s\n" pad x;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.Fold_right { collection; init; fold = { lam_var = Var x, _ty; body } } ->
    print_intro "Fold_right";
    Printf.printf "%s  collection:\n" pad;
    print_annotations ~indent:(indent + 2) collection;
    Printf.printf "%s  init:\n" pad;
    print_annotations ~indent:(indent + 2) init;
    Printf.printf "%s  lam_var=%s\n" pad x;
    Printf.printf "%s  body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.Let_tuple_in { components; rhs; in_ } ->
    print_intro "Let_tuple_in";
    Printf.printf
      "%s  components=[%s]\n"
      pad
      (String.concat ~sep:";" (List.map components ~f:(fun (Var v) -> v)));
    Printf.printf "%s  rhs:\n" pad;
    print_annotations ~indent:(indent + 2) rhs;
    Printf.printf "%s  in_:\n" pad;
    print_annotations ~indent:(indent + 2) in_
  | Expr.Tuple row ->
    print_intro "Tuple";
    let rec visit_row row =
      match row with
      | Row.Node sub -> List.iter ~f:visit_row sub
      | Row.Leaf (lbl, e) ->
        let lbl_str =
          match lbl with
          | Some (Row.Label s) -> s
          | None -> "<no-label>"
        in
        Printf.printf "%s  Leaf label=%s\n" pad lbl_str;
        print_annotations ~indent:(indent + 2) e
    in
    visit_row row
  | Expr.Proj (tup, _) ->
    print_intro "Proj";
    print_annotations ~indent:(indent + 2) tup
  | Expr.Update { tuple; update; _ } ->
    print_intro "Update";
    Printf.printf "%s  tuple:\n" pad;
    print_annotations ~indent:(indent + 2) tuple;
    Printf.printf "%s  update:\n" pad;
    print_annotations ~indent:(indent + 2) update
  | Expr.Raw_michelson { args; _ } ->
    print_intro "Raw_michelson";
    Printf.printf "%s  args:\n" pad;
    List.iter ~f:(fun x -> print_annotations ~indent:(indent + 2) x) args
  | Expr.Create_contract
      { code = { lam_var = Var lamv, _ty; body }
      ; delegate
      ; initial_balance
      ; initial_storage
      ; _
      } ->
    print_intro "Create_contract";
    Printf.printf "%s  lam_var=%s\n" pad lamv;
    Printf.printf "%s  delegate:\n" pad;
    print_annotations ~indent:(indent + 2) delegate;
    Printf.printf "%s  initial_balance:\n" pad;
    print_annotations ~indent:(indent + 2) initial_balance;
    Printf.printf "%s  initial_storage:\n" pad;
    print_annotations ~indent:(indent + 2) initial_storage;
    Printf.printf "%s  code body:\n" pad;
    print_annotations ~indent:(indent + 2) body
  | Expr.Global_constant { hash; args } ->
    print_intro "Global_constant";
    Printf.printf "%s  hash=%s\n" pad hash;
    Printf.printf "%s  args:\n" pad;
    List.iter ~f:(fun x -> print_annotations ~indent:(indent + 2) x) args
  | Expr.Deref (Mut_var x) ->
    print_intro "Deref";
    Printf.printf "%s  mut_var=%s\n" pad x
  | Expr.Variable (Var v) ->
    print_intro "Variable";
    Printf.printf "%s  var=%s\n" pad v
  | Expr.Inj _ -> print_intro "Inj (skipped printing details)"
  | Expr.Match _ -> print_intro "Match (skipped printing details)"
;;

let test_and_print title expr =
  Printf.printf "\n--- %s ---\n" title;
  let result = LV.compute_last_vars expr in
  print_annotations result;
  Printf.printf "\n";
  Test_nodes.test_expr expr
;;

(* -------------------------------------------------------------------- *)
(*
   let mut x = 100 in
   if (x = 100) then
   x <- x - 50
   else
   x <- x - 10
*)
let%expect_test "assign same var multiple branches" =
  let mut_x = mut_var "x" in
  let condition = eq (deref mut_x nat_ty) (nat 100) in
  let then_branch = assign mut_x (sub (deref mut_x nat_ty) (nat 50)) in
  let else_branch = assign mut_x (sub (deref mut_x nat_ty) (nat 10)) in
  let expr =
    let_mut_in
      mut_x
      ~rhs:(nat 100)
      ~in_:(if_bool condition ~then_:then_branch ~else_:else_branch)
  in
  test_and_print "assign same var multiple branches" expr;
  [%expect
    {|
    --- assign same var multiple branches ---
    Node: Let_mut_in
      luv = {x} nuv = {}
      let_var=x
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: If_bool
        luv = {x} nuv = {}
        condition:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Deref
              luv = {} nuv = {}
              mut_var=x
            Node: Const
              luv = {} nuv = {}
        if_true:
        Node: Assign
          luv = {x} nuv = {}
          let_var=x
          value:
          Node: Prim
            luv = {x} nuv = {}
            args:
            Node: Deref
              luv = {x} nuv = {}
              mut_var=x
            Node: Const
              luv = {} nuv = {}
        if_false:
        Node: Assign
          luv = {x} nuv = {}
          let_var=x
          value:
          Node: Prim
            luv = {x} nuv = {}
            args:
            Node: Deref
              luv = {x} nuv = {}
              mut_var=x
            Node: Const
              luv = {} nuv = {}

    { PUSH nat 100 ;
      PUSH nat 100 ;
      DUP 2 ;
      COMPARE ;
      EQ ;
      IF { PUSH nat 50 ; SWAP ; SUB ; DROP ; UNIT }
         { PUSH nat 10 ; SWAP ; SUB ; DROP ; UNIT } }

    Optimised:
    { PUSH nat 100 ;
      DUP ;
      DUP 2 ;
      COMPARE ;
      EQ ;
      IF { PUSH int -50 ; ADD ; DROP } { PUSH int -10 ; ADD ; DROP } ;
      UNIT } |}]
;;

(*
   let x = 10 in
   x
*)
let%expect_test "test_variable" =
  let expr = let_in (var "x") ~rhs:(int 10) ~in_:(variable (var "x") int_ty) in
  test_and_print "test_variable" expr;
  [%expect
    {|
    --- test_variable ---
    Node: Let_in
      luv = {x} nuv = {}
      let_var=x
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Variable
        luv = {x} nuv = {}
        var=x

    { PUSH int 10 }

    Optimised:
    { PUSH int 10 } |}]
;;

(*
   let unused = 10 in
   let x = 20 in
   let x = 10 in
   let x = x + 1 in
   x + unused
*)
let%expect_test "test_let_in" =
  let expr =
    let_in
      (var "unused")
      ~rhs:(int 10)
      ~in_:
        (let_in
           (var "x")
           ~rhs:(int 20)
           ~in_:
             (let_in
                (var "x")
                ~rhs:(int 10)
                ~in_:
                  (let_in
                     (var "x")
                     ~rhs:(add (variable (var "x") int_ty) (int 1))
                     ~in_:
                       (add (variable (var "x") int_ty) (variable (var "unused") int_ty)))))
  in
  test_and_print "test_let_in" expr;
  [%expect
    {|
    --- test_let_in ---
    Node: Let_in
      luv = {unused, x} nuv = {}
      let_var=unused
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {unused, x} nuv = {}
        let_var=x
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Let_in
          luv = {unused, x} nuv = {}
          let_var=x
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Let_in
            luv = {unused, x} nuv = {}
            let_var=x
            rhs:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Variable
                luv = {} nuv = {}
                var=x
              Node: Const
                luv = {} nuv = {}
            in_:
            Node: Prim
              luv = {unused, x} nuv = {}
              args:
              Node: Variable
                luv = {x} nuv = {}
                var=x
              Node: Variable
                luv = {unused} nuv = {}
                var=unused

    { PUSH int 10 ;
      PUSH int 20 ;
      PUSH int 10 ;
      PUSH int 1 ;
      DUP 2 ;
      ADD ;
      DIG 3 ;
      SWAP ;
      ADD ;
      SWAP ;
      DROP ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH int 21 } |}]
;;

(*
   let varA = 10 in
   let varB = 5 in
   (varA + varB) + varA
*)
let%expect_test "let_in multiple references no overshadow" =
  let expr =
    let_in
      (var "varA")
      ~rhs:(int 10)
      ~in_:
        (let_in
           (var "varB")
           ~rhs:(int 5)
           ~in_:
             (add
                (add (variable (var "varA") int_ty) (variable (var "varB") int_ty))
                (variable (var "varA") int_ty)))
  in
  test_and_print "let_in multiple references no overshadow" expr;
  [%expect
    {|
    --- let_in multiple references no overshadow ---
    Node: Let_in
      luv = {varA, varB} nuv = {}
      let_var=varA
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {varA, varB} nuv = {}
        let_var=varB
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Prim
          luv = {varA, varB} nuv = {}
          args:
          Node: Prim
            luv = {varA, varB} nuv = {}
            args:
            Node: Variable
              luv = {varA} nuv = {}
              var=varA
            Node: Variable
              luv = {varB} nuv = {}
              var=varB
          Node: Variable
            luv = {} nuv = {}
            var=varA

    { PUSH int 10 ; PUSH int 5 ; DUP 2 ; SWAP ; DIG 2 ; ADD ; ADD }

    Optimised:
    { PUSH int 10 ; PUSH int 5 ; DUP 2 ; SWAP ; DIG 2 ; ADD ; ADD } |}]
;;

(*
   let outerVar = 7 in
   fun (a:int) -> a + outerVar
*)
let%expect_test "test_lambda" =
  let expr =
    let_in
      (var "outerVar")
      ~rhs:(int 7)
      ~in_:
        (lambda
           (var "a", int_ty)
           ~body:(add (variable (var "a") int_ty) (variable (var "outerVar") int_ty)))
  in
  test_and_print "test_lambda" expr;
  [%expect
    {|
    --- test_lambda ---
    Node: Let_in
      luv = {a, outerVar} nuv = {}
      let_var=outerVar
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Lambda
        luv = {a, outerVar} nuv = {}
        lam_var=a
        body:
        Node: Prim
          luv = {a, outerVar} nuv = {}
          args:
          Node: Variable
            luv = {a} nuv = {}
            var=a
          Node: Variable
            luv = {outerVar} nuv = {}
            var=outerVar

    { PUSH int 7 ;
      LAMBDA (pair int int) int { UNPAIR ; SWAP ; ADD } ;
      SWAP ;
      APPLY }

    Optimised:
    { LAMBDA (pair int int) int { UNPAIR ; ADD } ; PUSH int 7 ; APPLY } |}]
;;

(*
   let y = 99 in
   let f = fun (a:int) -> a + y in
   let g = fun (a:int) ->
   (f (a * 2)) + y
   in
   g(10)
*)
let%expect_test "multiple lambdas referencing external vars" =
  let expr =
    let_in
      (var "y")
      ~rhs:(int 99)
      ~in_:
        (let_in
           (var "f")
           ~rhs:
             (lambda
                (var "a", int_ty)
                ~body:(add (variable (var "a") int_ty) (variable (var "y") int_ty)))
           ~in_:
             (let_in
                (var "g")
                ~rhs:
                  (lambda
                     (var "a", int_ty)
                     ~body:
                       (add
                          (app
                             (variable (var "f") (function_ty int_ty int_ty))
                             (mul (variable (var "a") int_ty) (int 2)))
                          (variable (var "y") int_ty)))
                ~in_:(app (variable (var "g") (function_ty int_ty int_ty)) (int 10))))
  in
  test_and_print "multiple lambdas referencing external vars" expr;
  [%expect
    {|
    --- multiple lambdas referencing external vars ---
    Node: Let_in
      luv = {a, f, g, y} nuv = {}
      let_var=y
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {a, f, g, y} nuv = {}
        let_var=f
        rhs:
        Node: Lambda
          luv = {a} nuv = {}
          lam_var=a
          body:
          Node: Prim
            luv = {a} nuv = {}
            args:
            Node: Variable
              luv = {a} nuv = {}
              var=a
            Node: Variable
              luv = {} nuv = {}
              var=y
        in_:
        Node: Let_in
          luv = {a, f, g, y} nuv = {}
          let_var=g
          rhs:
          Node: Lambda
            luv = {a, f, y} nuv = {}
            lam_var=a
            body:
            Node: Prim
              luv = {a, f, y} nuv = {}
              args:
              Node: App
                luv = {a, f} nuv = {}
                abs:
                Node: Variable
                  luv = {f} nuv = {}
                  var=f
                arg:
                Node: Prim
                  luv = {a} nuv = {}
                  args:
                  Node: Variable
                    luv = {a} nuv = {}
                    var=a
                  Node: Const
                    luv = {} nuv = {}
              Node: Variable
                luv = {y} nuv = {}
                var=y
          in_:
          Node: App
            luv = {g} nuv = {}
            abs:
            Node: Variable
              luv = {g} nuv = {}
              var=g
            arg:
            Node: Const
              luv = {} nuv = {}

    { PUSH int 99 ;
      LAMBDA (pair int int) int { UNPAIR ; DUP 1 ; DIG 2 ; ADD ; SWAP ; DROP } ;
      DUP 2 ;
      APPLY ;
      LAMBDA
        (pair (lambda int int) (pair int int))
        int
        { UNPAIR 3 ; SWAP ; PUSH int 2 ; DIG 3 ; MUL ; DIG 2 ; SWAP ; EXEC ; ADD } ;
      SWAP ;
      APPLY ;
      SWAP ;
      APPLY ;
      PUSH int 10 ;
      SWAP ;
      SWAP ;
      EXEC }

    Optimised:
    { PUSH int 99 ;
      LAMBDA (pair int int) int { UNPAIR ; ADD } ;
      DUP 2 ;
      APPLY ;
      LAMBDA
        (pair (lambda int int) (pair int int))
        int
        { UNPAIR 3 ; SWAP ; PUSH int 2 ; DIG 3 ; MUL ; DIG 2 ; SWAP ; EXEC ; ADD } ;
      SWAP ;
      APPLY ;
      SWAP ;
      APPLY ;
      PUSH int 10 ;
      EXEC } |}]
;;

(*
   let a = 1 in
   let b = 2 in
   fun (x:int) -> x + (a + b)
*)
let%expect_test "lambda referencing multiple distinct outside vars" =
  let expr =
    let_in
      (var "a")
      ~rhs:(int 1)
      ~in_:
        (let_in
           (var "b")
           ~rhs:(int 2)
           ~in_:
             (lambda
                (var "x", int_ty)
                ~body:
                  (add
                     (variable (var "x") int_ty)
                     (add (variable (var "a") int_ty) (variable (var "b") int_ty)))))
  in
  test_and_print "lambda referencing multiple distinct outside vars" expr;
  [%expect
    {|
    --- lambda referencing multiple distinct outside vars ---
    Node: Let_in
      luv = {a, b, x} nuv = {}
      let_var=a
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {a, b, x} nuv = {}
        let_var=b
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Lambda
          luv = {a, b, x} nuv = {}
          lam_var=x
          body:
          Node: Prim
            luv = {a, b, x} nuv = {}
            args:
            Node: Variable
              luv = {x} nuv = {}
              var=x
            Node: Prim
              luv = {a, b} nuv = {}
              args:
              Node: Variable
                luv = {a} nuv = {}
                var=a
              Node: Variable
                luv = {b} nuv = {}
                var=b

    { PUSH int 1 ;
      PUSH int 2 ;
      LAMBDA
        (pair int (pair int int))
        int
        { UNPAIR 3 ; SWAP ; SWAP ; ADD ; SWAP ; ADD } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      APPLY }

    Optimised:
    { PUSH int 1 ;
      PUSH int 2 ;
      LAMBDA (pair int (pair int int)) int { UNPAIR 3 ; ADD ; ADD } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      APPLY } |}]
;;

(*
   let outer = 1 in
   let rec f (n:int) =
   if n > 0 then
   f (n - 1)
   else
   outer
*)
let%expect_test "test_lambda_rec" =
  let body_expr =
    if_bool
      (gt (variable (var "n") int_ty) (int 0))
      ~then_:
        (app
           (variable (var "f") (function_ty int_ty int_ty))
           (sub (variable (var "n") int_ty) (int 1)))
      ~else_:(variable (var "outer") int_ty)
  in
  let lam_rec_expr = lambda_rec (var "f") (var "n", int_ty) ~body:body_expr in
  let expr = let_in (var "outer") ~rhs:(int 1) ~in_:lam_rec_expr in
  test_and_print "test_lambda_rec" expr;
  [%expect
    {|
    --- test_lambda_rec ---
    Node: Let_in
      luv = {f, n, outer} nuv = {}
      let_var=outer
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Lambda_rec
        luv = {f, n} nuv = {}
        mu_var=f
        lam_var=n
        body:
        Node: If_bool
          luv = {f, n, outer} nuv = {}
          condition:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Variable
                luv = {} nuv = {}
                var=n
              Node: Const
                luv = {} nuv = {}
          if_true:
          Node: App
            luv = {f, n} nuv = {outer}
            abs:
            Node: Variable
              luv = {f} nuv = {}
              var=f
            arg:
            Node: Prim
              luv = {n} nuv = {}
              args:
              Node: Variable
                luv = {n} nuv = {}
                var=n
              Node: Const
                luv = {} nuv = {}
          if_false:
          Node: Variable
            luv = {outer} nuv = {f, n}
            var=outer

    { PUSH int 1 ;
      LAMBDA_REC
        (pair int int)
        int
        { UNPAIR ;
          DIG 2 ;
          DUP 2 ;
          APPLY ;
          DUG 2 ;
          PUSH int 0 ;
          DUP 3 ;
          COMPARE ;
          GT ;
          IF { DROP ; PUSH int 1 ; SWAP ; SUB ; SWAP ; SWAP ; EXEC }
             { DIG 2 ; DROP ; SWAP ; DROP } } ;
      DUP 2 ;
      APPLY ;
      SWAP ;
      DROP }

    Optimised:
    { LAMBDA_REC
        (pair int int)
        int
        { UNPAIR ;
          DIG 2 ;
          DUP 2 ;
          APPLY ;
          DUG 2 ;
          DUP 2 ;
          GT ;
          IF { DROP ; PUSH int -1 ; ADD ; EXEC } { DUG 2 ; DROP 2 } } ;
      PUSH int 1 ;
      APPLY } |}]
;;

(*
   let unusedArg = 7 in
   let z = 10 in
   (fun (z:int) -> z + unusedArg)
   ( let z = 999 in 5 )
*)
let%expect_test "test_app" =
  let lam =
    lambda
      (var "z", int_ty)
      ~body:(add (variable (var "z") int_ty) (variable (var "unusedArg") int_ty))
  in
  let arg = let_in (var "z") ~rhs:(int 999) ~in_:(int 5) in
  let expr =
    let_in
      (var "unusedArg")
      ~rhs:(int 7)
      ~in_:(let_in (var "z") ~rhs:(int 10) ~in_:(app lam arg))
  in
  test_and_print "test_app" expr;
  [%expect
    {|
    --- test_app ---
    Node: Let_in
      luv = {unusedArg, z} nuv = {}
      let_var=unusedArg
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {unusedArg, z} nuv = {}
        let_var=z
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: App
          luv = {unusedArg, z} nuv = {}
          abs:
          Node: Lambda
            luv = {unusedArg, z} nuv = {}
            lam_var=z
            body:
            Node: Prim
              luv = {unusedArg, z} nuv = {}
              args:
              Node: Variable
                luv = {z} nuv = {}
                var=z
              Node: Variable
                luv = {unusedArg} nuv = {}
                var=unusedArg
          arg:
          Node: Let_in
            luv = {z} nuv = {}
            let_var=z
            rhs:
            Node: Const
              luv = {} nuv = {}
            in_:
            Node: Const
              luv = {} nuv = {}

    { PUSH int 7 ;
      PUSH int 10 ;
      PUSH int 999 ;
      PUSH int 5 ;
      SWAP ;
      DROP ;
      LAMBDA (pair int int) int { UNPAIR ; SWAP ; ADD } ;
      DIG 3 ;
      APPLY ;
      SWAP ;
      EXEC ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH int 7 ;
      PUSH int 5 ;
      LAMBDA (pair int int) int { UNPAIR ; ADD } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      EXEC } |}]
;;

(*
   let f = fun (p:(int * int)) -> fst(p) - snd(p) in
   let g = f(3) in
   let f = 999 in
   g(7)
*)
let%expect_test "partial apply overshadow f/g" =
  let pair_ty = Ast_builder.With_dummy.mk_tuple_ty [ int_ty; int_ty ] in
  let lam_f =
    lambda
      (var "p", pair_ty)
      ~body:(sub (car (variable (var "p") pair_ty)) (cdr (variable (var "p") pair_ty)))
  in
  let expr =
    let_in
      (var "f")
      ~rhs:lam_f
      ~in_:
        (let_in
           (var "g")
           ~rhs:(apply (int 3) (variable (var "f") (function_ty pair_ty int_ty)))
           ~in_:
             (let_in
                (var "f")
                ~rhs:(int 999)
                ~in_:(exec (int 7) (variable (var "g") (function_ty int_ty int_ty)))))
  in
  test_and_print "partial apply overshadow f/g" expr;
  [%expect
    {|
    --- partial apply overshadow f/g ---
    Node: Let_in
      luv = {f, g, p} nuv = {}
      let_var=f
      rhs:
      Node: Lambda
        luv = {p} nuv = {}
        lam_var=p
        body:
        Node: Prim
          luv = {p} nuv = {}
          args:
          Node: Prim
            luv = {p} nuv = {}
            args:
            Node: Variable
              luv = {p} nuv = {}
              var=p
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Variable
              luv = {} nuv = {}
              var=p
      in_:
      Node: Let_in
        luv = {f, g} nuv = {}
        let_var=g
        rhs:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Variable
            luv = {} nuv = {}
            var=f
        in_:
        Node: Let_in
          luv = {f, g} nuv = {}
          let_var=f
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Prim
            luv = {g} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Variable
              luv = {g} nuv = {}
              var=g

    { LAMBDA (pair int int) int { DUP 1 ; CDR ; SWAP ; CAR ; SUB } ;
      DUP 1 ;
      PUSH int 3 ;
      APPLY ;
      PUSH int 999 ;
      SWAP ;
      PUSH int 7 ;
      EXEC ;
      SWAP ;
      DROP ;
      SWAP ;
      DROP }

    Optimised:
    { LAMBDA (pair int int) int { UNPAIR ; SUB } ;
      PUSH int 3 ;
      APPLY ;
      PUSH int 7 ;
      EXEC } |}]
;;

(*
   let outerZ = 999 in
   let funF = fun (p:(int * int)) ->
   fst(p) + snd(p) + outerZ
   in
   let partial = funF(3) in
   partial(7)
*)
let%expect_test "partial apply referencing outer var" =
  let pair_ty = Ast_builder.With_dummy.mk_tuple_ty [ int_ty; int_ty ] in
  let lamF =
    lambda
      (var "p", pair_ty)
      ~body:
        (add
           (car (variable (var "p") pair_ty))
           (add (cdr (variable (var "p") pair_ty)) (variable (var "outerZ") int_ty)))
  in
  let expr =
    let_in
      (var "outerZ")
      ~rhs:(int 999)
      ~in_:
        (let_in
           (var "funF")
           ~rhs:lamF
           ~in_:
             (let_in
                (var "partial")
                ~rhs:(apply (int 3) (variable (var "funF") (function_ty pair_ty int_ty)))
                ~in_:(exec (int 7) (variable (var "partial") (function_ty int_ty int_ty)))))
  in
  test_and_print "partial apply referencing outer var" expr;
  [%expect
    {|
    --- partial apply referencing outer var ---
    Node: Let_in
      luv = {funF, outerZ, p, partial} nuv = {}
      let_var=outerZ
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {funF, outerZ, p, partial} nuv = {}
        let_var=funF
        rhs:
        Node: Lambda
          luv = {outerZ, p} nuv = {}
          lam_var=p
          body:
          Node: Prim
            luv = {outerZ, p} nuv = {}
            args:
            Node: Prim
              luv = {p} nuv = {}
              args:
              Node: Variable
                luv = {p} nuv = {}
                var=p
            Node: Prim
              luv = {outerZ} nuv = {}
              args:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Variable
                  luv = {} nuv = {}
                  var=p
              Node: Variable
                luv = {outerZ} nuv = {}
                var=outerZ
        in_:
        Node: Let_in
          luv = {funF, partial} nuv = {}
          let_var=partial
          rhs:
          Node: Prim
            luv = {funF} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Variable
              luv = {funF} nuv = {}
              var=funF
          in_:
          Node: Prim
            luv = {partial} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Variable
              luv = {partial} nuv = {}
              var=partial

    { PUSH int 999 ;
      LAMBDA
        (pair int (pair int int))
        int
        { UNPAIR ; DUP 2 ; CDR ; ADD ; SWAP ; CAR ; ADD } ;
      SWAP ;
      APPLY ;
      PUSH int 3 ;
      APPLY ;
      PUSH int 7 ;
      EXEC }

    Optimised:
    { LAMBDA
        (pair int (pair int int))
        int
        { UNPAIR ; DUP 2 ; CDR ; ADD ; SWAP ; CAR ; ADD } ;
      PUSH int 999 ;
      APPLY ;
      PUSH int 3 ;
      APPLY ;
      PUSH int 7 ;
      EXEC } |}]
;;

(*
   let outer = 10 in
   let mut x = 0 in
   let ignore1 = x <- x + outer in
   let f = fun (n:int) ->
   let g = fun (z:int) ->
   let ignore_g = x <- x + z in
   x
   in
   g(n + outer)
   in
   f(5)
*)
let%expect_test "mixed let_in, let_mut_in, nested lambdas referencing overshadow" =
  let mut_x = mut_var "x" in
  let assign_x_outer =
    assign mut_x (add (deref mut_x int_ty) (variable (var "outer") int_ty))
  in
  let lam_g =
    lambda
      (var "z", int_ty)
      ~body:
        (let_in
           (var "ignore_g")
           ~rhs:(assign mut_x (add (deref mut_x int_ty) (variable (var "z") int_ty)))
           ~in_:(deref mut_x int_ty))
  in
  let lam_f =
    lambda
      (var "n", int_ty)
      ~body:
        (let_in
           (var "g")
           ~rhs:lam_g
           ~in_:
             (app
                (variable (var "g") (function_ty int_ty int_ty))
                (add (variable (var "n") int_ty) (variable (var "outer") int_ty))))
  in
  let expr =
    let_in
      (var "outer")
      ~rhs:(int 10)
      ~in_:
        (let_mut_in
           mut_x
           ~rhs:(int 0)
           ~in_:
             (let_in
                (var "ignore1")
                ~rhs:assign_x_outer
                ~in_:
                  (let_in
                     (var "f")
                     ~rhs:lam_f
                     ~in_:(app (variable (var "f") (function_ty int_ty int_ty)) (int 5)))))
  in
  test_and_print "mixed let_in, let_mut_in, nested lambdas referencing overshadow" expr;
  [%expect
    {|
    --- mixed let_in, let_mut_in, nested lambdas referencing overshadow ---
    Node: Let_in
      luv = {f, g, ignore1, ignore_g, n, outer, x, z} nuv = {}
      let_var=outer
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_mut_in
        luv = {f, g, ignore1, ignore_g, n, outer, x, z} nuv = {}
        let_var=x
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Let_in
          luv = {f, g, ignore1, ignore_g, n, outer, x, z} nuv = {ignore1}
          let_var=ignore1
          rhs:
          Node: Assign
            luv = {} nuv = {}
            let_var=x
            value:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Deref
                luv = {} nuv = {}
                mut_var=x
              Node: Variable
                luv = {} nuv = {}
                var=outer
          in_:
          Node: Let_in
            luv = {f, g, ignore_g, n, outer, x, z} nuv = {}
            let_var=f
            rhs:
            Node: Lambda
              luv = {g, ignore_g, n, outer, x, z} nuv = {}
              lam_var=n
              body:
              Node: Let_in
                luv = {g, ignore_g, n, outer, x, z} nuv = {}
                let_var=g
                rhs:
                Node: Lambda
                  luv = {ignore_g, x, z} nuv = {}
                  lam_var=z
                  body:
                  Node: Let_in
                    luv = {ignore_g, x, z} nuv = {ignore_g}
                    let_var=ignore_g
                    rhs:
                    Node: Assign
                      luv = {z} nuv = {}
                      let_var=x
                      value:
                      Node: Prim
                        luv = {z} nuv = {}
                        args:
                        Node: Deref
                          luv = {} nuv = {}
                          mut_var=x
                        Node: Variable
                          luv = {z} nuv = {}
                          var=z
                    in_:
                    Node: Deref
                      luv = {x} nuv = {}
                      mut_var=x
                in_:
                Node: App
                  luv = {g, n, outer} nuv = {}
                  abs:
                  Node: Variable
                    luv = {g} nuv = {}
                    var=g
                  arg:
                  Node: Prim
                    luv = {n, outer} nuv = {}
                    args:
                    Node: Variable
                      luv = {n} nuv = {}
                      var=n
                    Node: Variable
                      luv = {outer} nuv = {}
                      var=outer
            in_:
            Node: App
              luv = {f} nuv = {}
              abs:
              Node: Variable
                luv = {f} nuv = {}
                var=f
              arg:
              Node: Const
                luv = {} nuv = {}

    { PUSH int 10 ;
      PUSH int 0 ;
      DUP 2 ;
      DUP 2 ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP ;
      LAMBDA
        (pair int (pair int int))
        int
        { UNPAIR 3 ;
          LAMBDA
            (pair int int)
            int
            { UNPAIR ; SWAP ; DUP 2 ; ADD ; DUG 1 ; DIG 0 ; DROP ; UNIT ; DROP } ;
          DIG 2 ;
          APPLY ;
          SWAP ;
          DIG 2 ;
          ADD ;
          SWAP ;
          SWAP ;
          EXEC } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      APPLY ;
      PUSH int 5 ;
      SWAP ;
      SWAP ;
      EXEC }

    Optimised:
    { PUSH int 10 ;
      PUSH int 0 ;
      DUP 2 ;
      ADD ;
      LAMBDA
        (pair int (pair int int))
        int
        { UNPAIR 3 ;
          LAMBDA (pair int int) int { UNPAIR ; ADD } ;
          DIG 2 ;
          APPLY ;
          SWAP ;
          DIG 2 ;
          ADD ;
          EXEC } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      APPLY ;
      PUSH int 5 ;
      EXEC } |}]
;;

(*
   42
*)
let%expect_test "test_const" =
  let expr = int 42 in
  test_and_print "test_const" expr;
  [%expect
    {|
    --- test_const ---
    Node: Const
      luv = {} nuv = {}

    { PUSH int 42 }

    Optimised:
    { PUSH int 42 } |}]
;;

(*
   let x = 20 in
   let y = 30 in
   (let x = 10 in x) + y
*)
let%expect_test "test_prim_add" =
  let lhs = let_in (var "x") ~rhs:(int 10) ~in_:(variable (var "x") int_ty) in
  let rhs = variable (var "y") int_ty in
  let expr =
    let_in
      (var "x")
      ~rhs:(int 20)
      ~in_:(let_in (var "y") ~rhs:(int 30) ~in_:(add lhs rhs))
  in
  test_and_print "test_prim_add" expr;
  [%expect
    {|
    --- test_prim_add ---
    Node: Let_in
      luv = {x, y} nuv = {}
      let_var=x
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {x, y} nuv = {}
        let_var=y
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Prim
          luv = {x, y} nuv = {}
          args:
          Node: Let_in
            luv = {x} nuv = {}
            let_var=x
            rhs:
            Node: Const
              luv = {} nuv = {}
            in_:
            Node: Variable
              luv = {x} nuv = {}
              var=x
          Node: Variable
            luv = {y} nuv = {}
            var=y

    { PUSH int 20 ; PUSH int 30 ; PUSH int 10 ; DIG 2 ; DROP ; ADD }

    Optimised:
    { PUSH int 40 } |}]
;;

(*
   let mut m = 100 in
   let m = 999 in
   m <- 1
*)
let%expect_test "test_let_mut_in" =
  let expr =
    let_mut_in
      (mut_var "m")
      ~rhs:(int 100)
      ~in_:(let_in (var "m") ~rhs:(int 999) ~in_:(assign (mut_var "m") (int 1)))
  in
  test_and_print "test_let_mut_in" expr;
  [%expect
    {|
    --- test_let_mut_in ---
    Node: Let_mut_in
      luv = {m} nuv = {m}
      let_var=m
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {m} nuv = {m}
        let_var=m
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Assign
          luv = {m} nuv = {m}
          let_var=m
          value:
          Node: Const
            luv = {} nuv = {}

    { PUSH int 100 ; DROP ; PUSH int 999 ; DROP ; PUSH int 1 ; DROP ; UNIT }

    Optimised:
    { UNIT } |}]
;;

(*
   let varX = 1 in
   let mut varM = 0 in
   let temp1 = varM <- (varM + 1) in
   let temp2 = varM <- (varM + varX) in
   varM
*)
let%expect_test "let_mut_in basic multiple assignment" =
  let mutM = mut_var "varM" in
  let assign1 = assign mutM (add (deref mutM int_ty) (int 1)) in
  let assign2 = assign mutM (add (deref mutM int_ty) (variable (var "varX") int_ty)) in
  let expr =
    let_in
      (var "varX")
      ~rhs:(int 1)
      ~in_:
        (let_mut_in
           mutM
           ~rhs:(int 0)
           ~in_:
             (let_in
                (var "temp1")
                ~rhs:assign1
                ~in_:(let_in (var "temp2") ~rhs:assign2 ~in_:(deref mutM int_ty))))
  in
  test_and_print "let_mut_in basic multiple assignment" expr;
  [%expect
    {|
    --- let_mut_in basic multiple assignment ---
    Node: Let_in
      luv = {temp1, temp2, varM, varX} nuv = {}
      let_var=varX
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_mut_in
        luv = {temp1, temp2, varM, varX} nuv = {}
        let_var=varM
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Let_in
          luv = {temp1, temp2, varM, varX} nuv = {temp1}
          let_var=temp1
          rhs:
          Node: Assign
            luv = {} nuv = {}
            let_var=varM
            value:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Deref
                luv = {} nuv = {}
                mut_var=varM
              Node: Const
                luv = {} nuv = {}
          in_:
          Node: Let_in
            luv = {temp2, varM, varX} nuv = {temp2}
            let_var=temp2
            rhs:
            Node: Assign
              luv = {varX} nuv = {}
              let_var=varM
              value:
              Node: Prim
                luv = {varX} nuv = {}
                args:
                Node: Deref
                  luv = {} nuv = {}
                  mut_var=varM
                Node: Variable
                  luv = {varX} nuv = {}
                  var=varX
            in_:
            Node: Deref
              luv = {varM} nuv = {}
              mut_var=varM

    { PUSH int 1 ;
      PUSH int 0 ;
      PUSH int 1 ;
      DUP 2 ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP ;
      SWAP ;
      DUP 2 ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP }

    Optimised:
    { PUSH int 2 } |}]
;;

(*
   let mut x = 7 in
   let x = 100 in
   x
*)
let%expect_test "test_deref" =
  let expr =
    let_mut_in
      (mut_var "x")
      ~rhs:(int 7)
      ~in_:(let_in (var "x") ~rhs:(int 100) ~in_:(deref (mut_var "x") int_ty))
  in
  test_and_print "test_deref" expr;
  [%expect
    {|
    --- test_deref ---
    Node: Let_mut_in
      luv = {x} nuv = {}
      let_var=x
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {x} nuv = {}
        let_var=x
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Deref
          luv = {x} nuv = {}
          mut_var=x

    { PUSH int 7 ; PUSH int 100 ; SWAP ; DROP }

    Optimised:
    { PUSH int 100 } |}]
;;

(*
   assign referencing overshadow var + unused var:
   let unused = 1 in
   let mut m = 0 in
   m <- (unused + m)
*)
let%expect_test "test_assign" =
  let expr =
    let_in
      (var "unused")
      ~rhs:(int 1)
      ~in_:
        (let_mut_in
           (mut_var "m")
           ~rhs:(int 0)
           ~in_:
             (assign
                (mut_var "m")
                (add (variable (var "unused") int_ty) (deref (mut_var "m") int_ty))))
  in
  test_and_print "test_assign" expr;
  [%expect
    {|
    --- test_assign ---
    Node: Let_in
      luv = {m, unused} nuv = {}
      let_var=unused
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_mut_in
        luv = {m, unused} nuv = {}
        let_var=m
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Assign
          luv = {m, unused} nuv = {}
          let_var=m
          value:
          Node: Prim
            luv = {m, unused} nuv = {}
            args:
            Node: Variable
              luv = {unused} nuv = {}
              var=unused
            Node: Deref
              luv = {m} nuv = {}
              mut_var=m

    { PUSH int 1 ; PUSH int 0 ; SWAP ; ADD ; DROP ; UNIT }

    Optimised:
    { UNIT } |}]
;;

(*
   let mut x = 0 in
   let ignore1 = x <- (
   let ignore =
   let_mut y = 999 in
   x <- (y + 1);
   in
   x
   ) in
   let ignore2 = x <- (x + 5) in
   x
*)
let%expect_test "nested assigns inside assigns" =
  let mut_x = mut_var "x" in
  let nested_assign =
    let_mut_in
      (mut_var "y")
      ~rhs:(int 999)
      ~in_:(assign (mut_var "x") (add (deref (mut_var "y") int_ty) (int 1)))
  in
  let first_assign =
    assign
      mut_x
      (let_in (var "ignore") ~rhs:nested_assign ~in_:(deref (mut_var "x") int_ty))
  in
  let second_assign = assign mut_x (add (deref mut_x int_ty) (int 5)) in
  let final_expr =
    let_mut_in
      mut_x
      ~rhs:(int 0)
      ~in_:
        (let_in
           (var "ignore1")
           ~rhs:first_assign
           ~in_:(let_in (var "ignore2") ~rhs:second_assign ~in_:(deref mut_x int_ty)))
  in
  test_and_print "nested assigns inside assigns" final_expr;
  [%expect
    {|
    --- nested assigns inside assigns ---
    Node: Let_mut_in
      luv = {ignore, ignore1, ignore2, x, y} nuv = {}
      let_var=x
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {ignore, ignore1, ignore2, x, y} nuv = {ignore1}
        let_var=ignore1
        rhs:
        Node: Assign
          luv = {ignore, y} nuv = {}
          let_var=x
          value:
          Node: Let_in
            luv = {ignore, y} nuv = {ignore}
            let_var=ignore
            rhs:
            Node: Let_mut_in
              luv = {y} nuv = {}
              let_var=y
              rhs:
              Node: Const
                luv = {} nuv = {}
              in_:
              Node: Assign
                luv = {y} nuv = {}
                let_var=x
                value:
                Node: Prim
                  luv = {y} nuv = {}
                  args:
                  Node: Deref
                    luv = {y} nuv = {}
                    mut_var=y
                  Node: Const
                    luv = {} nuv = {}
            in_:
            Node: Deref
              luv = {} nuv = {}
              mut_var=x
        in_:
        Node: Let_in
          luv = {ignore2, x} nuv = {ignore2}
          let_var=ignore2
          rhs:
          Node: Assign
            luv = {} nuv = {}
            let_var=x
            value:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Deref
                luv = {} nuv = {}
                mut_var=x
              Node: Const
                luv = {} nuv = {}
          in_:
          Node: Deref
            luv = {x} nuv = {}
            mut_var=x

    { PUSH int 0 ;
      PUSH int 999 ;
      PUSH int 1 ;
      SWAP ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP ;
      DUP 1 ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP ;
      PUSH int 5 ;
      DUP 2 ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP }

    Optimised:
    { PUSH int 1005 } |}]
;;

(*
   if_bool referencing distinct vars in branches:
   let c = false in
   let x = 10 in
   let y = 20 in
   if (c < 10) then x else y
*)
let%expect_test "test_if_bool" =
  let cond = lt (variable (var "c") int_ty) (int 10) in
  let then_ = variable (var "x") int_ty in
  let else_ = variable (var "y") int_ty in
  let expr =
    let_in
      (var "c")
      ~rhs:(bool false)
      ~in_:
        (let_in
           (var "x")
           ~rhs:(int 10)
           ~in_:(let_in (var "y") ~rhs:(int 20) ~in_:(if_bool cond ~then_ ~else_)))
  in
  test_and_print "test_if_bool" expr;
  [%expect
    {|
    --- test_if_bool ---
    Node: Let_in
      luv = {c, x, y} nuv = {}
      let_var=c
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {c, x, y} nuv = {}
        let_var=x
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Let_in
          luv = {c, x, y} nuv = {}
          let_var=y
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: If_bool
            luv = {c, x, y} nuv = {}
            condition:
            Node: Prim
              luv = {c} nuv = {}
              args:
              Node: Prim
                luv = {c} nuv = {}
                args:
                Node: Variable
                  luv = {c} nuv = {}
                  var=c
                Node: Const
                  luv = {} nuv = {}
            if_true:
            Node: Variable
              luv = {x} nuv = {y}
              var=x
            if_false:
            Node: Variable
              luv = {y} nuv = {x}
              var=y

    { PUSH bool False ;
      PUSH int 10 ;
      PUSH int 20 ;
      PUSH int 10 ;
      DIG 3 ;
      COMPARE ;
      LT ;
      IF { DROP } { SWAP ; DROP } }

    Optimised:
    { PUSH bool False ;
      PUSH int 10 ;
      PUSH int 20 ;
      PUSH int 10 ;
      DIG 3 ;
      COMPARE ;
      LT ;
      IF { DROP } { SWAP ; DROP } } |}]
;;

(*
   let condVar = 0 in
   let thenVar = 10 in
   let elseVar = 20 in
   if (condVar = 0) then thenVar + 100 else elseVar + 200
*)
let%expect_test "if_bool distinct var usage, no overshadow" =
  let condition = eq (variable (var "condVar") int_ty) (int 0) in
  let then_ = add (variable (var "thenVar") int_ty) (int 100) in
  let else_ = add (variable (var "elseVar") int_ty) (int 200) in
  let expr =
    let_in
      (var "condVar")
      ~rhs:(int 0)
      ~in_:
        (let_in
           (var "thenVar")
           ~rhs:(int 10)
           ~in_:
             (let_in (var "elseVar") ~rhs:(int 20) ~in_:(if_bool condition ~then_ ~else_)))
  in
  test_and_print "if_bool distinct var usage, no overshadow" expr;
  [%expect
    {|
    --- if_bool distinct var usage, no overshadow ---
    Node: Let_in
      luv = {condVar, elseVar, thenVar} nuv = {}
      let_var=condVar
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {condVar, elseVar, thenVar} nuv = {}
        let_var=thenVar
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Let_in
          luv = {condVar, elseVar, thenVar} nuv = {}
          let_var=elseVar
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: If_bool
            luv = {condVar, elseVar, thenVar} nuv = {}
            condition:
            Node: Prim
              luv = {condVar} nuv = {}
              args:
              Node: Prim
                luv = {condVar} nuv = {}
                args:
                Node: Variable
                  luv = {condVar} nuv = {}
                  var=condVar
                Node: Const
                  luv = {} nuv = {}
            if_true:
            Node: Prim
              luv = {thenVar} nuv = {elseVar}
              args:
              Node: Variable
                luv = {thenVar} nuv = {}
                var=thenVar
              Node: Const
                luv = {} nuv = {}
            if_false:
            Node: Prim
              luv = {elseVar} nuv = {thenVar}
              args:
              Node: Variable
                luv = {elseVar} nuv = {}
                var=elseVar
              Node: Const
                luv = {} nuv = {}

    { PUSH int 0 ;
      PUSH int 10 ;
      PUSH int 20 ;
      PUSH int 0 ;
      DIG 3 ;
      COMPARE ;
      EQ ;
      IF { DROP ; PUSH int 100 ; SWAP ; ADD }
         { SWAP ; DROP ; PUSH int 200 ; SWAP ; ADD } }

    Optimised:
    { PUSH int 0 ;
      PUSH int 10 ;
      PUSH int 20 ;
      DIG 2 ;
      EQ ;
      IF { DROP ; PUSH int 100 ; ADD } { SWAP ; DROP ; PUSH int 200 ; ADD } } |}]
;;

(*
   let x = 1 in
   if (x < 10) then
   if (x = 5) then
   let x = 999 in
   x
   else
   let mut x = 2 in
   x <- (x + 100)
   else
   x
*)
let%expect_test "complex nested if_bool overshadow" =
  let condition1 = lt (variable (var "x") int_ty) (int 10) in
  let then_branch =
    let condition2 = eq (variable (var "x") int_ty) (int 5) in
    let then_let = let_in (var "x") ~rhs:(int 999) ~in_:(variable (var "x") int_ty) in
    let else_mut =
      let_mut_in
        (mut_var "x")
        ~rhs:(int 2)
        ~in_:(assign (mut_var "x") (add (deref (mut_var "x") int_ty) (int 100)))
    in
    if_bool condition2 ~then_:then_let ~else_:else_mut
  in
  let else_branch = variable (var "x") int_ty in
  let expr =
    let_in
      (var "x")
      ~rhs:(int 1)
      ~in_:(if_bool condition1 ~then_:then_branch ~else_:else_branch)
  in
  test_and_print "complex nested if_bool overshadow" expr;
  [%expect
    {|
    --- complex nested if_bool overshadow ---
    Node: Let_in
      luv = {x} nuv = {}
      let_var=x
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: If_bool
        luv = {x} nuv = {}
        condition:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Variable
              luv = {} nuv = {}
              var=x
            Node: Const
              luv = {} nuv = {}
        if_true:
        Node: If_bool
          luv = {x} nuv = {}
          condition:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Variable
                luv = {} nuv = {}
                var=x
              Node: Const
                luv = {} nuv = {}
          if_true:
          Node: Let_in
            luv = {x} nuv = {}
            let_var=x
            rhs:
            Node: Const
              luv = {} nuv = {}
            in_:
            Node: Variable
              luv = {x} nuv = {}
              var=x
          if_false:
          Node: Let_mut_in
            luv = {x} nuv = {}
            let_var=x
            rhs:
            Node: Const
              luv = {} nuv = {}
            in_:
            Node: Assign
              luv = {x} nuv = {}
              let_var=x
              value:
              Node: Prim
                luv = {x} nuv = {}
                args:
                Node: Deref
                  luv = {x} nuv = {}
                  mut_var=x
                Node: Const
                  luv = {} nuv = {}
        if_false:
        Node: Variable
          luv = {x} nuv = {}
          var=x

    { PUSH int 1 ;
      PUSH int 10 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      IF { PUSH int 5 ;
           DUP 2 ;
           COMPARE ;
           EQ ;
           IF { PUSH int 999 ; SWAP ; DROP }
              { PUSH int 2 ; PUSH int 100 ; SWAP ; ADD ; SWAP ; DROP ; DROP ; UNIT } }
         {} }

    Optimised:
    { PUSH int 1 ;
      PUSH int 10 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      IF { PUSH int 5 ; COMPARE ; EQ ; IF { PUSH int 999 } { UNIT } } {} } |}]
;;

(*
   complicated if_bool + while + deref + overshadow:
   let mut counter = 0 in
   while (counter < 3) do
   if (counter = 1) then
   let x = counter in x
   else
   counter <- (counter + 10)
   done;
   let x = counter in x + 1
*)
let%expect_test "complicated if_bool + while + deref + overshadow" =
  let mut_counter = mut_var "counter" in
  let body =
    if_bool
      (eq (deref mut_counter int_ty) (int 1))
      ~then_:
        (let_in
           (var "x")
           ~rhs:(deref mut_counter int_ty)
           ~in_:(variable (var "x") int_ty))
      ~else_:(assign mut_counter (add (deref mut_counter int_ty) (int 10)))
  in
  let while_expr = while_ (lt (deref mut_counter int_ty) (int 3)) ~body in
  let after_while =
    let_in
      (var "x")
      ~rhs:(deref mut_counter int_ty)
      ~in_:(add (variable (var "x") int_ty) (int 1))
  in
  let expr =
    let_mut_in
      mut_counter
      ~rhs:(int 0)
      ~in_:(let_in (var "ignore_wh") ~rhs:while_expr ~in_:after_while)
  in
  test_and_print "complicated if_bool + while + deref + overshadow" expr;
  [%expect
    {|
    --- complicated if_bool + while + deref + overshadow ---
    Node: Let_mut_in
      luv = {counter, ignore_wh, x} nuv = {}
      let_var=counter
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {counter, ignore_wh, x} nuv = {ignore_wh}
        let_var=ignore_wh
        rhs:
        Node: While
          luv = {x} nuv = {}
          cond:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Deref
                luv = {} nuv = {}
                mut_var=counter
              Node: Const
                luv = {} nuv = {}
          body:
          Node: If_bool
            luv = {x} nuv = {}
            condition:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Deref
                  luv = {} nuv = {}
                  mut_var=counter
                Node: Const
                  luv = {} nuv = {}
            if_true:
            Node: Let_in
              luv = {x} nuv = {}
              let_var=x
              rhs:
              Node: Deref
                luv = {} nuv = {}
                mut_var=counter
              in_:
              Node: Variable
                luv = {x} nuv = {}
                var=x
            if_false:
            Node: Assign
              luv = {} nuv = {x}
              let_var=counter
              value:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Deref
                  luv = {} nuv = {}
                  mut_var=counter
                Node: Const
                  luv = {} nuv = {}
        in_:
        Node: Let_in
          luv = {counter, x} nuv = {}
          let_var=x
          rhs:
          Node: Deref
            luv = {counter} nuv = {}
            mut_var=counter
          in_:
          Node: Prim
            luv = {x} nuv = {}
            args:
            Node: Variable
              luv = {x} nuv = {}
              var=x
            Node: Const
              luv = {} nuv = {}

    { PUSH int 0 ;
      PUSH int 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH int 1 ;
             DUP 2 ;
             COMPARE ;
             EQ ;
             IF { DUP 1 } { PUSH int 10 ; DUP 2 ; ADD ; DUG 1 ; DIG 0 ; DROP ; UNIT } ;
             DROP ;
             PUSH int 3 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      UNIT ;
      DROP ;
      PUSH int 1 ;
      SWAP ;
      ADD }

    Optimised:
    { PUSH int 0 ;
      PUSH int 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH int 1 ;
             DUP 2 ;
             COMPARE ;
             EQ ;
             IF {} { PUSH int 10 ; ADD } ;
             PUSH int 3 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      PUSH int 1 ;
      ADD } |}]
;;

(*
   if_none overshadow in none branch, overshadow in some:
   let maybeVal = 0 in
   let outer = 100 in
   match maybeVal with
   None -> ( let tmpVal = 999 in 1 )
   | Some v -> outer + v
*)
let%expect_test "test_if_none" =
  let subject = variable (var "maybeVal") (option_ty int_ty) in
  let none_ = let_in (var "tmpVal") ~rhs:(int 999) ~in_:(int 1) in
  let some_body = add (variable (var "outer") int_ty) (variable (var "v") int_ty) in
  let expr =
    let_in
      (var "maybeVal")
      ~rhs:(int 0)
      ~in_:
        (let_in
           (var "outer")
           ~rhs:(int 100)
           ~in_:
             (if_none
                subject
                ~none:none_
                ~some:{ lam_var = var "v", int_ty; body = some_body }))
  in
  test_and_print "test_if_none" expr;
  [%expect
    {|
    --- test_if_none ---
    Node: Let_in
      luv = {maybeVal, outer, tmpVal, v} nuv = {}
      let_var=maybeVal
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {maybeVal, outer, tmpVal, v} nuv = {}
        let_var=outer
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: If_none
          luv = {maybeVal, outer, tmpVal, v} nuv = {}
          subject:
          Node: Variable
            luv = {maybeVal} nuv = {}
            var=maybeVal
          if_none:
          Node: Let_in
            luv = {tmpVal} nuv = {outer, tmpVal, v}
            let_var=tmpVal
            rhs:
            Node: Const
              luv = {} nuv = {}
            in_:
            Node: Const
              luv = {} nuv = {}
          if_some lam_var=v
          if_some body:
          Node: Prim
            luv = {outer, v} nuv = {tmpVal}
            args:
            Node: Variable
              luv = {outer} nuv = {}
              var=outer
            Node: Variable
              luv = {v} nuv = {}
              var=v

    { PUSH int 0 ;
      PUSH int 100 ;
      SWAP ;
      IF_NONE { DROP ; PUSH int 999 ; DROP ; PUSH int 1 } { SWAP ; ADD } }

    Optimised:
    { PUSH int 100 ; PUSH int 0 ; IF_NONE { DROP ; PUSH int 1 } { ADD } } |}]
;;

(*
   if_cons overshadow lam_var1 lam_var2:
   let lst = 0::[] in
   let hd = 1 in
   let hd2 = 2 in
   match lst with
   [] -> 999
   | hd::hd2 -> hd + hd2
*)
let%expect_test "test_if_cons" =
  let lst = variable (var "lst") (list_ty nat_ty) in
  let empty_expr = int 999 in
  let nonempty_body = add (variable (var "hd") nat_ty) (variable (var "hd2") nat_ty) in
  let expr =
    let_in
      (var "lst")
      ~rhs:(cons (int 0) (nil nat_ty))
      ~in_:
        (let_in
           (var "hd")
           ~rhs:(int 1)
           ~in_:
             (let_in
                (var "hd2")
                ~rhs:(int 2)
                ~in_:
                  (if_cons
                     lst
                     ~empty:empty_expr
                     ~nonempty:
                       { lam_var1 = var "hd", nat_ty
                       ; lam_var2 = var "hd2", list_ty nat_ty
                       ; body = nonempty_body
                       })))
  in
  test_and_print "test_if_cons" expr;
  [%expect
    {|
    --- test_if_cons ---
    Node: Let_in
      luv = {hd, hd2, lst} nuv = {}
      let_var=lst
      rhs:
      Node: Prim
        luv = {} nuv = {}
        args:
        Node: Const
          luv = {} nuv = {}
        Node: Prim
          luv = {} nuv = {}
          args:
      in_:
      Node: Let_in
        luv = {hd, hd2, lst} nuv = {}
        let_var=hd
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Let_in
          luv = {hd, hd2, lst} nuv = {}
          let_var=hd2
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: If_cons
            luv = {hd, hd2, lst} nuv = {}
            subject:
            Node: Variable
              luv = {lst} nuv = {}
              var=lst
            if_empty:
            Node: Const
              luv = {} nuv = {hd, hd2}
            if_nonempty lam_var1=hd lam_var2=hd2
            if_nonempty body:
            Node: Prim
              luv = {hd, hd2} nuv = {}
              args:
              Node: Variable
                luv = {hd} nuv = {}
                var=hd
              Node: Variable
                luv = {hd2} nuv = {}
                var=hd2

    { NIL nat ;
      PUSH int 0 ;
      CONS ;
      PUSH int 1 ;
      PUSH int 2 ;
      DIG 2 ;
      IF_CONS
        { SWAP ; SWAP ; ADD ; DIG 2 ; DROP ; SWAP ; DROP }
        { SWAP ; DROP ; DROP ; PUSH int 999 } }

    Optimised:
    { NIL nat ; PUSH nat 0 ; CONS ; IF_CONS { ADD } { PUSH int 999 } } |}]
;;

(*
   if_left overshadow lam_var in left or right:
   let s = Left(0 : nat) in
   match s with
   Left(l : int) ->
   let t = 999 in
   l
   | Right(r : nat) ->
   r + 1
*)
let%expect_test "test_if_left" =
  let or_val =
    variable
      (var "s")
      (or_ty (mk_row [ Row.Leaf (None, int_ty); Row.Leaf (None, nat_ty) ]))
  in
  let left_body = let_in (var "t") ~rhs:(int 999) ~in_:(variable (var "l") int_ty) in
  let right_body = add (variable (var "r") nat_ty) (int 1) in
  let expr =
    let_in
      (var "s")
      ~rhs:(left (None, None, nat_ty) (int 0))
      ~in_:
        (if_left
           or_val
           ~left:{ lam_var = var "l", int_ty; body = left_body }
           ~right:{ lam_var = var "r", nat_ty; body = right_body })
  in
  test_and_print "test_if_left" expr;
  [%expect
    {|
    --- test_if_left ---
    Node: Let_in
      luv = {l, r, s, t} nuv = {}
      let_var=s
      rhs:
      Node: Prim
        luv = {} nuv = {}
        args:
        Node: Const
          luv = {} nuv = {}
      in_:
      Node: If_left
        luv = {l, r, s, t} nuv = {}
        subject:
        Node: Variable
          luv = {s} nuv = {}
          var=s
        if_left lam_var=l
        Node: Let_in
          luv = {l, t} nuv = {r, t}
          let_var=t
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Variable
            luv = {l} nuv = {}
            var=l
        if_right lam_var=r
        Node: Prim
          luv = {r} nuv = {l, t}
          args:
          Node: Variable
            luv = {r} nuv = {}
            var=r
          Node: Const
            luv = {} nuv = {}

    { PUSH int 0 ;
      LEFT nat ;
      IF_LEFT { PUSH int 999 ; DROP } { PUSH int 1 ; SWAP ; ADD } }

    Optimised:
    { PUSH int 0 ; LEFT nat ; IF_LEFT {} { PUSH int 1 ; ADD } } |}]
;;

(*
   while referencing overshadow var + nested let_in:
   let mut m = 0 in
   while (m < 5) do
   let q = 123 in
   m <- q
   done
*)
let%expect_test "test_while" =
  let body_expr =
    let_in
      (var "q")
      ~rhs:(int 123)
      ~in_:(assign (mut_var "m") (variable (var "q") int_ty))
  in
  let expr =
    let_mut_in
      (mut_var "m")
      ~rhs:(int 0)
      ~in_:(while_ (lt (deref (mut_var "m") int_ty) (int 5)) ~body:body_expr)
  in
  test_and_print "test_while" expr;
  [%expect
    {|
    --- test_while ---
    Node: Let_mut_in
      luv = {m, q} nuv = {}
      let_var=m
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: While
        luv = {m, q} nuv = {}
        cond:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Deref
              luv = {} nuv = {}
              mut_var=m
            Node: Const
              luv = {} nuv = {}
        body:
        Node: Let_in
          luv = {q} nuv = {}
          let_var=q
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Assign
            luv = {q} nuv = {}
            let_var=m
            value:
            Node: Variable
              luv = {q} nuv = {}
              var=q

    { PUSH int 0 ;
      PUSH int 5 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH int 123 ;
             DUG 1 ;
             DIG 0 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH int 5 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      DROP ;
      UNIT }

    Optimised:
    { PUSH int 0 ;
      PUSH int 5 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { DROP ; PUSH int 123 ; PUSH int 5 ; DUP 2 ; COMPARE ; LT } ;
      DROP ;
      UNIT } |}]
;;

(*
   let limit = 10 in
   let step = 2 in
   let mut i = 0 in
   while (i < limit) do
   i <- i + step
   done
*)
let%expect_test "while referencing outside variables" =
  let mutI = mut_var "i" in
  let limitVar = variable (var "limit") int_ty in
  let stepVar = variable (var "step") int_ty in
  let body_expr = assign mutI (add (deref mutI int_ty) stepVar) in
  let expr =
    let_in
      (var "limit")
      ~rhs:(int 10)
      ~in_:
        (let_in
           (var "step")
           ~rhs:(int 2)
           ~in_:
             (let_mut_in
                mutI
                ~rhs:(int 0)
                ~in_:(while_ (lt (deref mutI int_ty) limitVar) ~body:body_expr)))
  in
  test_and_print "while referencing outside variables" expr;
  [%expect
    {|
    --- while referencing outside variables ---
    Node: Let_in
      luv = {i, limit, step} nuv = {}
      let_var=limit
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {i, limit, step} nuv = {}
        let_var=step
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Let_mut_in
          luv = {i, limit, step} nuv = {}
          let_var=i
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: While
            luv = {i, limit, step} nuv = {}
            cond:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Deref
                  luv = {} nuv = {}
                  mut_var=i
                Node: Variable
                  luv = {} nuv = {}
                  var=limit
            body:
            Node: Assign
              luv = {} nuv = {}
              let_var=i
              value:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Deref
                  luv = {} nuv = {}
                  mut_var=i
                Node: Variable
                  luv = {} nuv = {}
                  var=step

    { PUSH int 10 ;
      PUSH int 2 ;
      PUSH int 0 ;
      DUP 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { DUP 2 ;
             DUP 2 ;
             ADD ;
             DUG 1 ;
             DIG 0 ;
             DROP ;
             UNIT ;
             DROP ;
             DUP 3 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      DROP ;
      SWAP ;
      DROP ;
      DROP ;
      UNIT }

    Optimised:
    { PUSH int 10 ;
      PUSH int 2 ;
      PUSH int 0 ;
      DUP 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { DUP 2 ; ADD ; DUP 3 ; DUP 2 ; COMPARE ; LT } ;
      DROP 3 ;
      UNIT } |}]
;;

(*
   while_left (Left 10)
   (flag : int) ->
   let flag = 999 in
   Right(123)
*)
let%expect_test "test_while_left" =
  let start_val = left (None, None, int_ty) (int 10) in
  let expr =
    while_left
      start_val
      ~body:
        { lam_var = var "flag", int_ty
        ; body =
            let_in (var "flag") ~rhs:(int 999) ~in_:(right (None, None, int_ty) (int 123))
        }
  in
  test_and_print "test_while_left" expr;
  [%expect
    {|
    --- test_while_left ---
    Node: While_left
      luv = {flag} nuv = {}
      cond:
      Node: Prim
        luv = {} nuv = {}
        args:
        Node: Const
          luv = {} nuv = {}
      lam_var=flag
      body:
      Node: Let_in
        luv = {flag} nuv = {flag}
        let_var=flag
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}

    { PUSH int 10 ;
      LEFT int ;
      LEFT int ;
      LOOP_LEFT { DROP ; PUSH int 999 ; DROP ; PUSH int 123 ; RIGHT int } }

    Optimised:
    { PUSH int 10 ;
      LEFT int ;
      LEFT int ;
      LOOP_LEFT { DROP ; PUSH int 123 ; RIGHT int } } |}]
;;

(*
   for referencing index overshadow body var + outer var:
   let mut acc = 0 in
   for i from 0 while i<5 do
   acc <- i
   i <- i+1
   done
*)
let%expect_test "test_for" =
  let idx = mut_var "i" in
  let body_expr = assign (mut_var "acc") (variable (var "i") int_ty) in
  let expr =
    let_mut_in
      (mut_var "acc")
      ~rhs:(int 0)
      ~in_:
        (for_
           idx
           ~init:(int 0)
           ~cond:(lt (deref idx int_ty) (int 5))
           ~update:(assign idx (add (deref idx int_ty) (int 1)))
           ~body:body_expr)
  in
  test_and_print "test_for" expr;
  [%expect
    {|
    --- test_for ---
    Node: Let_mut_in
      luv = {acc, i} nuv = {acc}
      let_var=acc
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: For
        luv = {acc, i} nuv = {}
        index=i
        init:
        Node: Const
          luv = {} nuv = {}
        cond:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Deref
              luv = {} nuv = {}
              mut_var=i
            Node: Const
              luv = {} nuv = {}
        update:
        Node: Assign
          luv = {} nuv = {}
          let_var=i
          value:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Deref
              luv = {} nuv = {}
              mut_var=i
            Node: Const
              luv = {} nuv = {}
        body:
        Node: Assign
          luv = {} nuv = {acc}
          let_var=acc
          value:
          Node: Variable
            luv = {} nuv = {}
            var=i

    { PUSH int 0 ;
      DROP ;
      PUSH int 0 ;
      PUSH int 5 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { DUP 1 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH int 1 ;
             DUP 2 ;
             ADD ;
             DUG 1 ;
             DIG 0 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH int 5 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      DROP ;
      UNIT }

    Optimised:
    { PUSH int 0 ;
      PUSH int 5 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH int 1 ; ADD ; PUSH int 5 ; DUP 2 ; COMPARE ; LT } ;
      DROP ;
      UNIT } |}]
;;

(*
   let mut i = 0 in
   let f = (fun (p : (int,int)) -> fst(p) + snd(p) + i ) in
   for i from i while i<4 do
   let partial = f(2) in
   partial(3)
   i <- i+1
   done
*)
let%expect_test "for_ partial apply overshadow in body" =
  let idx = mut_var "i" in
  let pair_ty = Ast_builder.With_dummy.mk_tuple_ty [ int_ty; int_ty ] in
  let lam_f =
    lambda
      (var "p", pair_ty)
      ~body:
        (add
           (car (variable (var "p") pair_ty))
           (add (cdr (variable (var "p") pair_ty)) (deref idx int_ty)))
  in
  let body_expr =
    let_in
      (var "partial")
      ~rhs:(apply (int 2) lam_f)
      ~in_:(app (variable (var "partial") (function_ty int_ty int_ty)) (int 3))
  in
  let expr =
    let_mut_in
      idx
      ~rhs:(int 0)
      ~in_:
        (let_in
           (var "f")
           ~rhs:lam_f
           ~in_:
             (for_
                idx
                ~init:(deref idx int_ty)
                ~cond:(lt (deref idx int_ty) (int 4))
                ~update:(assign idx (add (deref idx int_ty) (int 1)))
                ~body:body_expr))
  in
  test_and_print "for_ partial apply overshadow in body" expr;
  [%expect
    {|
    --- for_ partial apply overshadow in body ---
    Node: Let_mut_in
      luv = {f, i, p, partial} nuv = {}
      let_var=i
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {f, i, p, partial} nuv = {f}
        let_var=f
        rhs:
        Node: Lambda
          luv = {p} nuv = {}
          lam_var=p
          body:
          Node: Prim
            luv = {p} nuv = {}
            args:
            Node: Prim
              luv = {p} nuv = {}
              args:
              Node: Variable
                luv = {p} nuv = {}
                var=p
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Variable
                  luv = {} nuv = {}
                  var=p
              Node: Deref
                luv = {} nuv = {}
                mut_var=i
        in_:
        Node: For
          luv = {i, p, partial} nuv = {}
          index=i
          init:
          Node: Deref
            luv = {} nuv = {}
            mut_var=i
          cond:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Deref
                luv = {} nuv = {}
                mut_var=i
              Node: Const
                luv = {} nuv = {}
          update:
          Node: Assign
            luv = {} nuv = {}
            let_var=i
            value:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Deref
                luv = {} nuv = {}
                mut_var=i
              Node: Const
                luv = {} nuv = {}
          body:
          Node: Let_in
            luv = {p, partial} nuv = {}
            let_var=partial
            rhs:
            Node: Prim
              luv = {p} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Lambda
                luv = {p} nuv = {}
                lam_var=p
                body:
                Node: Prim
                  luv = {p} nuv = {}
                  args:
                  Node: Prim
                    luv = {p} nuv = {}
                    args:
                    Node: Variable
                      luv = {p} nuv = {}
                      var=p
                  Node: Prim
                    luv = {} nuv = {}
                    args:
                    Node: Prim
                      luv = {} nuv = {}
                      args:
                      Node: Variable
                        luv = {} nuv = {}
                        var=p
                    Node: Deref
                      luv = {} nuv = {}
                      mut_var=i
            in_:
            Node: App
              luv = {partial} nuv = {}
              abs:
              Node: Variable
                luv = {partial} nuv = {}
                var=partial
              arg:
              Node: Const
                luv = {} nuv = {}

    { PUSH int 0 ;
      LAMBDA
        (pair int (pair int int))
        int
        { UNPAIR ; DUP 1 ; DUP 3 ; CDR ; ADD ; DIG 2 ; CAR ; ADD ; SWAP ; DROP } ;
      DUP 2 ;
      APPLY ;
      DROP ;
      DUP 1 ;
      PUSH int 4 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { LAMBDA
               (pair int (pair int int))
               int
               { UNPAIR ; DUP 1 ; DUP 3 ; CDR ; ADD ; DIG 2 ; CAR ; ADD ; SWAP ; DROP } ;
             DUP 2 ;
             APPLY ;
             PUSH int 2 ;
             APPLY ;
             PUSH int 3 ;
             SWAP ;
             SWAP ;
             EXEC ;
             DROP ;
             PUSH int 1 ;
             DUP 2 ;
             ADD ;
             DUG 1 ;
             DIG 0 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH int 4 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      DROP ;
      DROP ;
      UNIT }

    Optimised:
    { PUSH int 0 ;
      PUSH int 4 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { LAMBDA
               (pair int (pair int int))
               int
               { UNPAIR ; DUP 2 ; CDR ; ADD ; SWAP ; CAR ; ADD } ;
             DUP 2 ;
             APPLY ;
             PUSH int 2 ;
             APPLY ;
             PUSH int 3 ;
             EXEC ;
             DROP ;
             PUSH int 1 ;
             ADD ;
             PUSH int 4 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      DROP ;
      UNIT } |}]
;;

(*
   for_each overshadow lam_var, referencing outer var:
   let outer = 100 in
   for_each [1;2]
   (elem : int) ->
   let elem = 999 in
   elem + outer
*)
let%expect_test "test_for_each" =
  let collection = cons (int 1) (cons (int 2) (nil int_ty)) in
  let lam_body =
    let_in
      (var "elem")
      ~rhs:(int 999)
      ~in_:(add (variable (var "elem") int_ty) (variable (var "outer") int_ty))
  in
  let expr =
    let_in
      (var "outer")
      ~rhs:(int 100)
      ~in_:(for_each collection ~body:{ lam_var = var "elem", int_ty; body = lam_body })
  in
  test_and_print "test_for_each" expr;
  [%expect
    {|
    --- test_for_each ---
    Node: Let_in
      luv = {elem, outer} nuv = {}
      let_var=outer
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: For_each
        luv = {elem, outer} nuv = {}
        collection:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Prim
              luv = {} nuv = {}
              args:
        lam_var=elem
        body:
        Node: Let_in
          luv = {elem} nuv = {}
          let_var=elem
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Prim
            luv = {elem} nuv = {}
            args:
            Node: Variable
              luv = {elem} nuv = {}
              var=elem
            Node: Variable
              luv = {} nuv = {}
              var=outer

    { PUSH int 100 ;
      NIL int ;
      PUSH int 2 ;
      CONS ;
      PUSH int 1 ;
      CONS ;
      ITER { PUSH int 999 ; DUP 3 ; SWAP ; ADD ; SWAP ; DROP ; DROP } ;
      DROP ;
      UNIT }

    Optimised:
    { PUSH int 100 ;
      PUSH (list int) { 1 ; 2 } ;
      ITER { DROP ; PUSH int 999 ; DUP 2 ; ADD ; DROP } ;
      DROP ;
      UNIT } |}]
;;

(*
   map overshadow lam_var, referencing outer var
   let outer = 10 in
   map [1;2] (x : nat) ->
   let x=999 in
   x + outer
*)
let%expect_test "test_map" =
  let collection = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let lam_body =
    let_in
      (var "x")
      ~rhs:(nat 999)
      ~in_:(add (variable (var "x") nat_ty) (variable (var "outer") nat_ty))
  in
  let expr =
    let_in
      (var "outer")
      ~rhs:(nat 10)
      ~in_:(map collection ~map:{ lam_var = var "x", nat_ty; body = lam_body })
  in
  test_and_print "test_map" expr;
  [%expect
    {|
    --- test_map ---
    Node: Let_in
      luv = {outer, x} nuv = {}
      let_var=outer
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Map
        luv = {outer, x} nuv = {}
        collection:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Prim
              luv = {} nuv = {}
              args:
        lam_var=x
        body:
        Node: Let_in
          luv = {x} nuv = {}
          let_var=x
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Prim
            luv = {x} nuv = {}
            args:
            Node: Variable
              luv = {x} nuv = {}
              var=x
            Node: Variable
              luv = {} nuv = {}
              var=outer

    { PUSH nat 10 ;
      NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      MAP { PUSH nat 999 ; DUP 3 ; SWAP ; ADD ; SWAP ; DROP } ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH nat 10 ;
      PUSH (list nat) { 1 ; 2 } ;
      MAP { DROP ; PUSH nat 999 ; DUP 2 ; ADD } ;
      SWAP ;
      DROP } |}]
;;

(*
   let x=999 in
   map [10;20] (v:int) ->
   ( let x=v in x+1 ) + x
*)
let%expect_test "map overshadow partial usage" =
  let list_expr = cons (int 10) (cons (int 20) (nil int_ty)) in
  let lam_body =
    add
      (let_in
         (var "x")
         ~rhs:(variable (var "v") int_ty)
         ~in_:(add (variable (var "x") int_ty) (int 1)))
      (variable (var "x") int_ty)
  in
  let expr =
    let_in
      (var "x")
      ~rhs:(int 999)
      ~in_:(map list_expr ~map:{ lam_var = var "v", int_ty; body = lam_body })
  in
  test_and_print "map overshadow partial usage" expr;
  [%expect
    {|
    --- map overshadow partial usage ---
    Node: Let_in
      luv = {v, x} nuv = {}
      let_var=x
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Map
        luv = {v, x} nuv = {}
        collection:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Prim
              luv = {} nuv = {}
              args:
        lam_var=v
        body:
        Node: Prim
          luv = {v, x} nuv = {}
          args:
          Node: Let_in
            luv = {v, x} nuv = {}
            let_var=x
            rhs:
            Node: Variable
              luv = {v} nuv = {}
              var=v
            in_:
            Node: Prim
              luv = {x} nuv = {}
              args:
              Node: Variable
                luv = {x} nuv = {}
                var=x
              Node: Const
                luv = {} nuv = {}
          Node: Variable
            luv = {} nuv = {}
            var=x

    { PUSH int 999 ;
      NIL int ;
      PUSH int 20 ;
      CONS ;
      PUSH int 10 ;
      CONS ;
      MAP { DUP 2 ; SWAP ; PUSH int 1 ; SWAP ; ADD ; DIG 2 ; DROP ; ADD } ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH int 999 ;
      PUSH (list int) { 10 ; 20 } ;
      MAP { PUSH int 1 ; ADD ; ADD } ;
      SWAP ;
      DROP } |}]
;;

(*
   let factor = 3 in
   map [10;20] (item:int) ->
   item + item + factor
*)
let%expect_test "map usage referencing outside var, multiple item usage" =
  let list_expr = cons (int 10) (cons (int 20) (nil int_ty)) in
  let lam_body =
    add
      (variable (var "item") int_ty)
      (add (variable (var "item") int_ty) (variable (var "factor") int_ty))
  in
  let expr =
    let_in
      (var "factor")
      ~rhs:(int 3)
      ~in_:(map list_expr ~map:{ lam_var = var "item", int_ty; body = lam_body })
  in
  test_and_print "map usage referencing outside var, multiple item usage" expr;
  [%expect
    {|
    --- map usage referencing outside var, multiple item usage ---
    Node: Let_in
      luv = {factor, item} nuv = {}
      let_var=factor
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Map
        luv = {factor, item} nuv = {}
        collection:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Prim
              luv = {} nuv = {}
              args:
        lam_var=item
        body:
        Node: Prim
          luv = {item} nuv = {}
          args:
          Node: Variable
            luv = {item} nuv = {}
            var=item
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Variable
              luv = {} nuv = {}
              var=item
            Node: Variable
              luv = {} nuv = {}
              var=factor

    { PUSH int 3 ;
      NIL int ;
      PUSH int 20 ;
      CONS ;
      PUSH int 10 ;
      CONS ;
      MAP { DUP 2 ; DUP 2 ; ADD ; SWAP ; ADD } ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH int 3 ;
      PUSH (list int) { 10 ; 20 } ;
      MAP { DUP 2 ; DUP 2 ; ADD ; ADD } ;
      SWAP ;
      DROP } |}]
;;

(*
   fold_left overshadow lam_var + referencing init:
   fold_left [1;2]
   init=0
   (acc:(int*int)) ->
   let acc=999 in
   fst(acc) + snd(acc)
*)
let%expect_test "test_fold_left" =
  let coll = cons (int 1) (cons (int 2) (nil int_ty)) in
  let init_expr = int 0 in
  let lam_body =
    let_in
      (var "acc")
      ~rhs:(int 999)
      ~in_:
        (add
           (car (variable (var "acc") (mk_tuple_ty [ int_ty; int_ty ])))
           (cdr (variable (var "acc") (mk_tuple_ty [ int_ty; int_ty ]))))
  in
  let expr =
    fold_left
      coll
      ~init:init_expr
      ~fold:{ lam_var = var "acc", mk_tuple_ty [ int_ty; int_ty ]; body = lam_body }
  in
  test_and_print "test_fold_left" expr;
  [%expect
    {|
    --- test_fold_left ---
    Node: Fold_left
      luv = {acc} nuv = {}
      collection:
      Node: Prim
        luv = {} nuv = {}
        args:
        Node: Const
          luv = {} nuv = {}
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Prim
            luv = {} nuv = {}
            args:
      init:
      Node: Const
        luv = {} nuv = {}
      lam_var=acc
      body:
      Node: Let_in
        luv = {acc} nuv = {}
        let_var=acc
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Prim
          luv = {acc} nuv = {}
          args:
          Node: Prim
            luv = {acc} nuv = {}
            args:
            Node: Variable
              luv = {acc} nuv = {}
              var=acc
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Variable
              luv = {} nuv = {}
              var=acc

    { PUSH int 0 ;
      NIL int ;
      PUSH int 2 ;
      CONS ;
      PUSH int 1 ;
      CONS ;
      ITER { SWAP ;
             PAIR ;
             PUSH int 999 ;
             DUP 1 ;
             CDR ;
             SWAP ;
             CAR ;
             ADD ;
             SWAP ;
             DROP } }

    Optimised:
    { PUSH int 0 ;
      PUSH (list int) { 1 ; 2 } ;
      ITER { DROP 2 ; PUSH int 999 ; UNPAIR ; ADD } } |}]
;;

(*
   let outside=7 in
   fold_left [1;2;3] init=0
   (acc,x) ->
   let x = fst(acc) + outside in
   x + x
*)
let%expect_test "fold_left overshadow inside body" =
  let coll = cons (int 1) (cons (int 2) (cons (int 3) (nil int_ty))) in
  let fold_body =
    let_in
      (var "x")
      ~rhs:
        (add
           (car (variable (var "acc") (mk_tuple_ty [ int_ty; int_ty ])))
           (variable (var "outside") int_ty))
      ~in_:(add (variable (var "x") int_ty) (variable (var "x") int_ty))
  in
  let expr =
    let_in
      (var "outside")
      ~rhs:(int 7)
      ~in_:
        (fold_left
           coll
           ~init:(int 0)
           ~fold:{ lam_var = var "acc", mk_tuple_ty [ int_ty; int_ty ]; body = fold_body })
  in
  test_and_print "fold_left overshadow inside body" expr;
  [%expect
    {|
    --- fold_left overshadow inside body ---
    Node: Let_in
      luv = {acc, outside, x} nuv = {}
      let_var=outside
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Fold_left
        luv = {acc, outside, x} nuv = {}
        collection:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Prim
                luv = {} nuv = {}
                args:
        init:
        Node: Const
          luv = {} nuv = {}
        lam_var=acc
        body:
        Node: Let_in
          luv = {acc, x} nuv = {}
          let_var=x
          rhs:
          Node: Prim
            luv = {acc} nuv = {}
            args:
            Node: Prim
              luv = {acc} nuv = {}
              args:
              Node: Variable
                luv = {acc} nuv = {}
                var=acc
            Node: Variable
              luv = {} nuv = {}
              var=outside
          in_:
          Node: Prim
            luv = {x} nuv = {}
            args:
            Node: Variable
              luv = {x} nuv = {}
              var=x
            Node: Variable
              luv = {} nuv = {}
              var=x

    { PUSH int 7 ;
      PUSH int 0 ;
      NIL int ;
      PUSH int 3 ;
      CONS ;
      PUSH int 2 ;
      CONS ;
      PUSH int 1 ;
      CONS ;
      ITER { SWAP ; PAIR ; DUP 2 ; SWAP ; CAR ; ADD ; DUP 1 ; SWAP ; ADD } ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH int 7 ;
      PUSH int 0 ;
      PUSH (list int) { 1 ; 2 ; 3 } ;
      ITER { SWAP ; PAIR ; DUP 2 ; SWAP ; CAR ; ADD ; DUP ; ADD } ;
      SWAP ;
      DROP } |}]
;;

(*
   let outside=5 in
   fold_left [1;2] init=0
   (acc,x) ->
   (fst(acc) + outside) + snd(acc)
*)
let%expect_test "fold_left referencing outside var" =
  let coll = cons (int 1) (cons (int 2) (nil int_ty)) in
  let fold_body =
    add
      (add
         (car (variable (var "acc") (mk_tuple_ty [ int_ty; int_ty ])))
         (variable (var "outside") int_ty))
      (cdr (variable (var "acc") (mk_tuple_ty [ int_ty; int_ty ])))
  in
  let expr =
    let_in
      (var "outside")
      ~rhs:(int 5)
      ~in_:
        (fold_left
           coll
           ~init:(int 0)
           ~fold:{ lam_var = var "acc", mk_tuple_ty [ int_ty; int_ty ]; body = fold_body })
  in
  test_and_print "fold_left referencing outside var" expr;
  [%expect
    {|
    --- fold_left referencing outside var ---
    Node: Let_in
      luv = {acc, outside} nuv = {}
      let_var=outside
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Fold_left
        luv = {acc, outside} nuv = {}
        collection:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Prim
              luv = {} nuv = {}
              args:
        init:
        Node: Const
          luv = {} nuv = {}
        lam_var=acc
        body:
        Node: Prim
          luv = {acc} nuv = {}
          args:
          Node: Prim
            luv = {acc} nuv = {}
            args:
            Node: Prim
              luv = {acc} nuv = {}
              args:
              Node: Variable
                luv = {acc} nuv = {}
                var=acc
            Node: Variable
              luv = {} nuv = {}
              var=outside
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Variable
              luv = {} nuv = {}
              var=acc

    { PUSH int 5 ;
      PUSH int 0 ;
      NIL int ;
      PUSH int 2 ;
      CONS ;
      PUSH int 1 ;
      CONS ;
      ITER { SWAP ; PAIR ; DUP 1 ; CDR ; DUP 3 ; DIG 2 ; CAR ; ADD ; ADD } ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH int 5 ;
      PUSH int 0 ;
      PUSH (list int) { 1 ; 2 } ;
      ITER { SWAP ; PAIR ; DUP ; CDR ; DUP 3 ; DIG 2 ; CAR ; ADD ; ADD } ;
      SWAP ;
      DROP } |}]
;;

(*
   fold_right overshadow lam_var + referencing outer var:
   let unused=666 in
   fold_right [1;2]
   init=0
   (acc:(int*int)) ->
   let acc=999 in
   snd(acc) - fst(acc)
*)
let%expect_test "test_fold_right" =
  let coll = cons (int 1) (cons (int 2) (nil int_ty)) in
  let init_expr = int 0 in
  let lam_body =
    let_in
      (var "acc")
      ~rhs:(int 999)
      ~in_:
        (sub
           (cdr (variable (var "acc") (mk_tuple_ty [ int_ty; int_ty ])))
           (car (variable (var "acc") (mk_tuple_ty [ int_ty; int_ty ]))))
  in
  let expr =
    let_in
      (var "unused")
      ~rhs:(int 666)
      ~in_:
        (fold_right
           coll
           ~init:init_expr
           ~fold:{ lam_var = var "acc", mk_tuple_ty [ int_ty; int_ty ]; body = lam_body })
  in
  test_and_print "test_fold_right" expr;
  [%expect
    {|
    --- test_fold_right ---
    Node: Let_in
      luv = {acc, unused} nuv = {unused}
      let_var=unused
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Fold_right
        luv = {acc} nuv = {}
        collection:
        Node: Prim
          luv = {} nuv = {}
          args:
          Node: Const
            luv = {} nuv = {}
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Const
              luv = {} nuv = {}
            Node: Prim
              luv = {} nuv = {}
              args:
        init:
        Node: Const
          luv = {} nuv = {}
        lam_var=acc
        body:
        Node: Let_in
          luv = {acc} nuv = {}
          let_var=acc
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Prim
            luv = {acc} nuv = {}
            args:
            Node: Prim
              luv = {acc} nuv = {}
              args:
              Node: Variable
                luv = {acc} nuv = {}
                var=acc
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Variable
                luv = {} nuv = {}
                var=acc

    { PUSH int 666 ;
      DROP ;
      PUSH int 0 ;
      NIL int ;
      PUSH int 2 ;
      CONS ;
      PUSH int 1 ;
      CONS ;
      NIL int ;
      SWAP ;
      ITER { CONS } ;
      ITER { PAIR ; PUSH int 999 ; DUP 1 ; CAR ; SWAP ; CDR ; SUB ; SWAP ; DROP } }

    Optimised:
    { PUSH int 0 ;
      NIL int ;
      PUSH (list int) { 1 ; 2 } ;
      ITER { CONS } ;
      ITER { DROP 2 ; PUSH int 999 ; UNPAIR ; SWAP ; SUB } } |}]
;;

(*
   let_tuple_in overshadow many components, referencing only some:
   let_tuple_in [a,b,a] = (1,2,3) in
   a + b
*)
let%expect_test "test_let_tuple_in" =
  let triple =
    tuple
      (Row.Node [ Row.Leaf (None, int 1); Row.Leaf (None, int 2); Row.Leaf (None, int 3) ])
  in
  let expr =
    let_tuple_in
      [ var "a"; var "b"; var "a" ] (* overshadow a *)
      ~rhs:triple
      ~in_:(add (variable (var "a") int_ty) (variable (var "b") int_ty))
  in
  test_and_print "test_let_tuple_in" expr;
  [%expect
    {|
    --- test_let_tuple_in ---
    Node: Let_tuple_in
      luv = {a, b} nuv = {}
      components=[a;b;a]
      rhs:
      Node: Tuple
        luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
      in_:
      Node: Prim
        luv = {a, b} nuv = {}
        args:
        Node: Variable
          luv = {a} nuv = {}
          var=a
        Node: Variable
          luv = {b} nuv = {}
          var=b

    { PUSH int 3 ;
      PUSH int 2 ;
      PUSH int 1 ;
      PAIR 3 ;
      UNPAIR 3 ;
      SWAP ;
      SWAP ;
      ADD ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH int 3 ; PUSH int 2 ; PUSH int 1 ; DIG 2 ; DROP ; ADD } |}]
;;

(*
   let triple = (1,2,3) in
   let_tuple_in [a,b,c] = triple in
   let unused=999 in
   a + b + c
*)
let%expect_test "let_tuple_in referencing multiple times, plus an unused" =
  let triple_expr =
    tuple
      (Row.Node [ Row.Leaf (None, int 1); Row.Leaf (None, int 2); Row.Leaf (None, int 3) ])
  in
  let expr =
    let_in
      (var "triple")
      ~rhs:triple_expr
      ~in_:
        (let_tuple_in
           [ var "a"; var "b"; var "c" ]
           ~rhs:(variable (var "triple") (mk_tuple_ty [ int_ty; int_ty; int_ty ]))
           ~in_:
             (let_in
                (var "unused")
                ~rhs:(int 999)
                ~in_:
                  (add
                     (variable (var "a") int_ty)
                     (add (variable (var "b") int_ty) (variable (var "c") int_ty)))))
  in
  test_and_print "let_tuple_in referencing multiple times, plus an unused" expr;
  [%expect
    {|
    --- let_tuple_in referencing multiple times, plus an unused ---
    Node: Let_in
      luv = {a, b, c, triple, unused} nuv = {}
      let_var=triple
      rhs:
      Node: Tuple
        luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
      in_:
      Node: Let_tuple_in
        luv = {a, b, c, triple, unused} nuv = {}
        components=[a;b;c]
        rhs:
        Node: Variable
          luv = {triple} nuv = {}
          var=triple
        in_:
        Node: Let_in
          luv = {a, b, c, unused} nuv = {unused}
          let_var=unused
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Prim
            luv = {a, b, c} nuv = {}
            args:
            Node: Variable
              luv = {a} nuv = {}
              var=a
            Node: Prim
              luv = {b, c} nuv = {}
              args:
              Node: Variable
                luv = {b} nuv = {}
                var=b
              Node: Variable
                luv = {c} nuv = {}
                var=c

    { PUSH int 3 ;
      PUSH int 2 ;
      PUSH int 1 ;
      PAIR 3 ;
      UNPAIR 3 ;
      PUSH int 999 ;
      DROP ;
      DIG 2 ;
      DIG 2 ;
      ADD ;
      SWAP ;
      ADD }

    Optimised:
    { PUSH int 3 ; PUSH int 2 ; PUSH int 1 ; DUG 2 ; ADD ; ADD } |}]
;;

(*
   tuple referencing multiple overshadow variables:
   let unused=999 in
   ( let x=10 in x,
   unused )
*)
let%expect_test "test_tuple" =
  let expr =
    let_in
      (var "unused")
      ~rhs:(int 999)
      ~in_:
        (tuple
           (Row.Node
              [ Row.Leaf
                  (None, let_in (var "x") ~rhs:(int 10) ~in_:(variable (var "x") int_ty))
              ; Row.Leaf (None, variable (var "unused") int_ty)
              ]))
  in
  test_and_print "test_tuple" expr;
  [%expect
    {|
    --- test_tuple ---
    Node: Let_in
      luv = {unused, x} nuv = {}
      let_var=unused
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Tuple
        luv = {unused, x} nuv = {}
        Leaf label=<no-label>
        Node: Let_in
          luv = {x} nuv = {}
          let_var=x
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Variable
            luv = {x} nuv = {}
            var=x
        Leaf label=<no-label>
        Node: Variable
          luv = {unused} nuv = {}
          var=unused

    { PUSH int 999 ; PUSH int 10 ; PAIR }

    Optimised:
    { PUSH int 999 ; PUSH int 10 ; PAIR } |}]
;;

(*
   proj overshadow referencing outer var + partial usage:
   let alpha=999 in
   (alpha, 999, 123).[0]
*)
let%expect_test "test_proj" =
  let triple =
    let_in
      (var "alpha")
      ~rhs:(int 999)
      ~in_:
        (tuple
           (Row.Node
              [ Row.Leaf (None, variable (var "alpha") int_ty)
              ; Row.Leaf (None, int 999)
              ; Row.Leaf (None, int 123)
              ]))
  in
  let expr = proj triple ~path:(Row.Path.Here [ 0 ]) in
  test_and_print "test_proj" expr;
  [%expect
    {|
    --- test_proj ---
    Node: Proj
      luv = {alpha} nuv = {}
      Node: Let_in
        luv = {alpha} nuv = {}
        let_var=alpha
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Tuple
          luv = {alpha} nuv = {}
          Leaf label=<no-label>
          Node: Variable
            luv = {alpha} nuv = {}
            var=alpha
          Leaf label=<no-label>
          Node: Const
            luv = {} nuv = {}
          Leaf label=<no-label>
          Node: Const
            luv = {} nuv = {}

    { PUSH int 999 ; PUSH int 123 ; PUSH int 999 ; DIG 2 ; PAIR 3 ; GET 1 }

    Optimised:
    { PUSH int 999 ; PUSH int 123 ; PUSH int 999 ; DIG 2 ; PAIR 3 ; CAR } |}]
;;

(*
   update overshadow with let_in on update value:
   update (10,20) index=1 with
   let u=999 in u
*)
let%expect_test "test_update" =
  let pair_expr = tuple (Row.Node [ Row.Leaf (None, int 10); Row.Leaf (None, int 20) ]) in
  let upd_expr = let_in (var "u") ~rhs:(int 999) ~in_:(variable (var "u") int_ty) in
  let expr = update_tuple pair_expr ~component:(Row.Path.Here [ 1 ]) ~update:upd_expr in
  test_and_print "test_update" expr;
  [%expect
    {|
    --- test_update ---
    Node: Update
      luv = {u} nuv = {}
      tuple:
      Node: Tuple
        luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
      update:
      Node: Let_in
        luv = {u} nuv = {}
        let_var=u
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Variable
          luv = {u} nuv = {}
          var=u

    { PUSH int 20 ;
      PUSH int 10 ;
      PAIR ;
      DUP 1 ;
      GET 2 ;
      DROP ;
      PUSH int 999 ;
      UPDATE 2 }

    Optimised:
    { PUSH int 20 ; PUSH int 10 ; PAIR ; PUSH int 999 ; UPDATE 2 } |}]
;;

(*
   let outsideU=100 in
   update (10,20) index=1 with
   let tmp=9 in tmp + outsideU
*)
let%expect_test "update_tuple referencing multiple outside vars in update" =
  let pair_expr = tuple (Row.Node [ Row.Leaf (None, int 10); Row.Leaf (None, int 20) ]) in
  let upd_expr =
    let_in
      (var "tmp")
      ~rhs:(int 9)
      ~in_:(add (variable (var "tmp") int_ty) (variable (var "outsideU") int_ty))
  in
  let expr =
    let_in
      (var "outsideU")
      ~rhs:(int 100)
      ~in_:(update_tuple pair_expr ~component:(Row.Path.Here [ 1 ]) ~update:upd_expr)
  in
  test_and_print "update_tuple referencing multiple outside vars in update" expr;
  [%expect
    {|
    --- update_tuple referencing multiple outside vars in update ---
    Node: Let_in
      luv = {outsideU, tmp} nuv = {}
      let_var=outsideU
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Update
        luv = {outsideU, tmp} nuv = {}
        tuple:
        Node: Tuple
          luv = {} nuv = {}
          Leaf label=<no-label>
          Node: Const
            luv = {} nuv = {}
          Leaf label=<no-label>
          Node: Const
            luv = {} nuv = {}
        update:
        Node: Let_in
          luv = {outsideU, tmp} nuv = {}
          let_var=tmp
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Prim
            luv = {outsideU, tmp} nuv = {}
            args:
            Node: Variable
              luv = {tmp} nuv = {}
              var=tmp
            Node: Variable
              luv = {outsideU} nuv = {}
              var=outsideU

    { PUSH int 100 ;
      PUSH int 20 ;
      PUSH int 10 ;
      PAIR ;
      DUP 1 ;
      GET 2 ;
      DROP ;
      PUSH int 9 ;
      DIG 2 ;
      SWAP ;
      ADD ;
      UPDATE 2 }

    Optimised:
    { PUSH int 100 ;
      PUSH int 20 ;
      PUSH int 10 ;
      PAIR ;
      PUSH int 9 ;
      DIG 2 ;
      ADD ;
      UPDATE 2 } |}]
;;

module MA = Lltz_michelson.Ast

let push_int n =
  Tezos_micheline.Micheline.Prim
    ( Ast_builder.Dummy_range.v
    , "PUSH"
    , [ Tezos_micheline.Micheline.Prim (Ast_builder.Dummy_range.v, "int", [], [])
      ; Tezos_micheline.Micheline.Int (Ast_builder.Dummy_range.v, Z.of_int n)
      ]
    , [] )
;;

(*
   RAW_MICHELSON overshadow in args 
  let x=10 in
    raw_michelson { PUSH int 42 } [
      let unusedR=7 in unusedR,
      x
    ] : int
*)
let%expect_test "test_raw_michelson" =
  let code_ast =
    Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [ push_int 42 ])
  in
  let arg1 = let_in (var "unusedR") ~rhs:(int 7) ~in_:(variable (var "unusedR") int_ty) in
  let arg2 = variable (var "x") int_ty in
  let expr =
    let_in (var "x") ~rhs:(int 10) ~in_:(raw_michelson code_ast [ arg1; arg2 ] int_ty)
  in
  test_and_print "test_raw_michelson" expr;
  [%expect
    {|
    --- test_raw_michelson ---
    Node: Let_in
      luv = {unusedR, x} nuv = {}
      let_var=x
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Raw_michelson
        luv = {unusedR, x} nuv = {}
        args:
        Node: Let_in
          luv = {unusedR} nuv = {}
          let_var=unusedR
          rhs:
          Node: Const
            luv = {} nuv = {}
          in_:
          Node: Variable
            luv = {unusedR} nuv = {}
            var=unusedR
        Node: Variable
          luv = {x} nuv = {}
          var=x

    { PUSH int 10 ; PUSH int 7 ; { PUSH int 42 } }

    Optimised:
    { PUSH int 10 ; PUSH int 7 ; PUSH int 42 } |}]
;;

(*
   raw_michelson { PUSH int 42 } [
    let x=10 in x,
    let x=999 in x
  ] : int
*)
let%expect_test "raw_michelson overshadow multiple args" =
  let code_ast =
    Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [ push_int 42 ])
  in
  let arg1 = let_in (var "x") ~rhs:(int 10) ~in_:(variable (var "x") int_ty) in
  let arg2 = let_in (var "x") ~rhs:(int 999) ~in_:(variable (var "x") int_ty) in
  let expr = raw_michelson code_ast [ arg1; arg2 ] int_ty in
  test_and_print "raw_michelson overshadow multiple args" expr;
  [%expect
    {|
    --- raw_michelson overshadow multiple args ---
    Node: Raw_michelson
      luv = {x} nuv = {}
      args:
      Node: Let_in
        luv = {x} nuv = {}
        let_var=x
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Variable
          luv = {x} nuv = {}
          var=x
      Node: Let_in
        luv = {x} nuv = {}
        let_var=x
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Variable
          luv = {x} nuv = {}
          var=x

    { PUSH int 999 ; PUSH int 10 ; { PUSH int 42 } }

    Optimised:
    { PUSH int 999 ; PUSH int 10 ; PUSH int 42 } |}]
;;

(*
   CREATE_CONTRACT overshadow code var, referencing multiple unused:
   let args=(1,2) in
   let bal=100mutez in
   let del=None in
   let unusedCC="no use" in
   create_contract
   storage=nat
   code = fun(args:(nat*nat)) ->
   let (p,s)=args in
   p + s
   delegate=del
   initial_balance=bal
   initial_storage=
   let args=99 in
   100
*)
let%expect_test "test_create_contract" =
  let param_storage_ty = mk_tuple_ty [ nat_ty; nat_ty ] in
  let code_body =
    let_tuple_in
      [ var "p"; var "s" ]
      ~rhs:(variable (var "args") param_storage_ty)
      ~in_:(add (variable (var "p") nat_ty) (variable (var "s") nat_ty))
  in
  let expr =
    let_in
      (var "args")
      ~rhs:(tuple (Row.Node [ Row.Leaf (None, int 1); Row.Leaf (None, int 2) ]))
      ~in_:
        (let_in
           (var "bal")
           ~rhs:(mutez 100)
           ~in_:
             (let_in
                (var "del")
                ~rhs:(none key_hash_ty)
                ~in_:
                  (let_in
                     (var "unusedCC")
                     ~rhs:(string "no use") (* overshadow or never used var *)
                     ~in_:
                       (create_contract
                          ~storage:nat_ty
                          ~code:
                            { lam_var = var "args", param_storage_ty; body = code_body }
                          ~delegate:(variable (var "del") (option_ty key_hash_ty))
                          ~initial_balance:(variable (var "bal") mutez_ty)
                          ~initial_storage:
                            (let_in (var "args") ~rhs:(nat 99) ~in_:(nat 100))))))
  in
  test_and_print "test_create_contract" expr;
  [%expect
    {|
    --- test_create_contract ---
    Node: Let_in
      luv = {args, bal, del, p, s, unusedCC} nuv = {}
      let_var=args
      rhs:
      Node: Tuple
        luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
        Leaf label=<no-label>
        Node: Const
          luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {args, bal, del, p, s, unusedCC} nuv = {}
        let_var=bal
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Let_in
          luv = {args, bal, del, p, s, unusedCC} nuv = {}
          let_var=del
          rhs:
          Node: Prim
            luv = {} nuv = {}
            args:
          in_:
          Node: Let_in
            luv = {args, bal, del, p, s, unusedCC} nuv = {unusedCC}
            let_var=unusedCC
            rhs:
            Node: Const
              luv = {} nuv = {}
            in_:
            Node: Create_contract
              luv = {args, bal, del, p, s} nuv = {}
              lam_var=args
              delegate:
              Node: Variable
                luv = {del} nuv = {}
                var=del
              initial_balance:
              Node: Variable
                luv = {bal} nuv = {}
                var=bal
              initial_storage:
              Node: Let_in
                luv = {args} nuv = {}
                let_var=args
                rhs:
                Node: Const
                  luv = {} nuv = {}
                in_:
                Node: Const
                  luv = {} nuv = {}
              code body:
              Node: Let_tuple_in
                luv = {args, p, s} nuv = {}
                components=[p;s]
                rhs:
                Node: Variable
                  luv = {args} nuv = {}
                  var=args
                in_:
                Node: Prim
                  luv = {p, s} nuv = {}
                  args:
                  Node: Variable
                    luv = {p} nuv = {}
                    var=p
                  Node: Variable
                    luv = {s} nuv = {}
                    var=s

    { PUSH int 2 ;
      PUSH int 1 ;
      PAIR ;
      PUSH mutez 100 ;
      NONE key_hash ;
      PUSH string "no use" ;
      DROP ;
      PUSH nat 99 ;
      PUSH nat 100 ;
      SWAP ;
      DROP ;
      DIG 2 ;
      DIG 2 ;
      CREATE_CONTRACT
        { parameter nat ;
          storage nat ;
          code { UNPAIR ; SWAP ; SWAP ; ADD ; SWAP ; DROP } } ;
      PAIR ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH mutez 100 ;
      NONE key_hash ;
      PUSH nat 100 ;
      DUG 2 ;
      CREATE_CONTRACT
        { parameter nat ; storage nat ; code { SWAP ; DROP ; UNPAIR ; ADD } } ;
      PAIR } |}]
;;

(*
   GLOBAL_CONSTANT overshadow multiple arguments:
   global_constant "SomeGlobalHash" [
    let x=1 in x,
    let x=99 in x
  ] : int
*)
let%expect_test "test_global_constant" =
  let arg1 = let_in (var "x") ~rhs:(int 1) ~in_:(variable (var "x") int_ty) in
  let arg2 = let_in (var "x") ~rhs:(int 99) ~in_:(variable (var "x") int_ty) in
  let expr = global_constant "SomeGlobalHash" [ arg1; arg2 ] int_ty in
  test_and_print "test_global_constant" expr;
  [%expect
    {|
    --- test_global_constant ---
    Node: Global_constant
      luv = {x} nuv = {}
      hash=SomeGlobalHash
      args:
      Node: Let_in
        luv = {x} nuv = {}
        let_var=x
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Variable
          luv = {x} nuv = {}
          var=x
      Node: Let_in
        luv = {x} nuv = {}
        let_var=x
        rhs:
        Node: Const
          luv = {} nuv = {}
        in_:
        Node: Variable
          luv = {x} nuv = {}
          var=x

    { PUSH int 99 ; PUSH int 1 ; constant "SomeGlobalHash" }

    Optimised:
    { PUSH int 99 ; PUSH int 1 ; constant "SomeGlobalHash" } |}]

let%expect_test "last used var in for each body" =
  let param_ty =
    or_ty
      (Node
         [ Leaf (Option.some @@ Row.Label "endorsement", list_ty nat_ty)
         ; Leaf (Option.some @@ Row.Label "proposal", nat_ty)
         ])
  in
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let param = left (Some "endorsement", Some "proposal", param_ty) lst in
  let expr =
    let_in
      (var "PROBLEM")
      ~rhs:(nat 0)
      ~in_:
        (let_in
           (var "_ignore")
           ~rhs:
             (if_left
                param
                ~left:
                  { lam_var = var "left_", list_ty nat_ty
                  ; body =
                      for_each
                        (variable (var "left_") (list_ty nat_ty))
                        ~body:
                          { lam_var = var "_iter", nat_ty
                          ; body = variable (var "PROBLEM") nat_ty
                          }
                  }
                ~right:
                  { lam_var = var "right_", nat_ty
                  ; body = variable (var "PROBLEM") nat_ty
                  })
           ~in_:unit) 
  in
  test_and_print "last used var in for each body" expr;
  [%expect {|
    --- last used var in for each body ---
    Node: Let_in
      luv = {PROBLEM, _ignore, _iter, left_, right_} nuv = {}
      let_var=PROBLEM
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {PROBLEM, _ignore, _iter, left_, right_} nuv = {_ignore}
        let_var=_ignore
        rhs:
        Node: If_left
          luv = {PROBLEM, _iter, left_, right_} nuv = {}
          subject:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Const
                  luv = {} nuv = {}
                Node: Prim
                  luv = {} nuv = {}
                  args:
          if_left lam_var=left_
          Node: For_each
            luv = {PROBLEM, _iter, left_} nuv = {}
            collection:
            Node: Variable
              luv = {left_} nuv = {}
              var=left_
            lam_var=_iter
            body:
            Node: Variable
              luv = {} nuv = {_iter}
              var=PROBLEM
          if_right lam_var=right_
          Node: Variable
            luv = {PROBLEM} nuv = {_iter, left_, right_}
            var=PROBLEM
        in_:
        Node: Const
          luv = {} nuv = {}

    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      LEFT (or (list %endorsement nat) (nat %proposal)) ;
      IF_LEFT { ITER { DROP ; DUP 1 ; DROP } ; DROP ; UNIT } { DROP } ;
      DROP ;
      UNIT }

    Optimised:
    { UNIT } |}]

let%expect_test "last used var in map body" =
  let param_ty =
    or_ty
      (Node
         [ Leaf (Option.some @@ Row.Label "endorsement", list_ty nat_ty)
         ; Leaf (Option.some @@ Row.Label "proposal", nat_ty)
         ])
  in
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let param = left (Some "endorsement", Some "proposal", param_ty) lst in
  let expr =
    let_in
      (var "PROBLEM")
      ~rhs:(nat 0)
      ~in_:
        (let_in
           (var "_ignore")
           ~rhs:
             (if_left
                param
                ~left:
                  { lam_var = var "left_", list_ty nat_ty
                  ; body =
                      map
                        (variable (var "left_") (list_ty nat_ty))
                        ~map:
                          { lam_var = var "_iter", nat_ty
                          ; body = variable (var "PROBLEM") nat_ty
                          }
                  }
                ~right:
                  { lam_var = var "right_", nat_ty
                  ; body = variable (var "PROBLEM") nat_ty
                  })
           ~in_:unit) 
  in
  test_and_print "last used var in map body" expr;
  [%expect {|
    --- last used var in map body ---
    Node: Let_in
      luv = {PROBLEM, _ignore, _iter, left_, right_} nuv = {}
      let_var=PROBLEM
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {PROBLEM, _ignore, _iter, left_, right_} nuv = {_ignore}
        let_var=_ignore
        rhs:
        Node: If_left
          luv = {PROBLEM, _iter, left_, right_} nuv = {}
          subject:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Const
                  luv = {} nuv = {}
                Node: Prim
                  luv = {} nuv = {}
                  args:
          if_left lam_var=left_
          Node: Map
            luv = {PROBLEM, _iter, left_} nuv = {}
            collection:
            Node: Variable
              luv = {left_} nuv = {}
              var=left_
            lam_var=_iter
            body:
            Node: Variable
              luv = {} nuv = {_iter}
              var=PROBLEM
          if_right lam_var=right_
          Node: Variable
            luv = {PROBLEM} nuv = {_iter, left_, right_}
            var=PROBLEM
        in_:
        Node: Const
          luv = {} nuv = {}

    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      LEFT (or (list %endorsement nat) (nat %proposal)) ;
      IF_LEFT { MAP { DROP ; DUP 1 } ; SWAP ; DROP } { DROP } ;
      DROP ;
      UNIT }

    Optimised:
    { UNIT } |}]


let%expect_test "last used var in fold_left body" =
  let param_ty =
    or_ty
      (Node
         [ Leaf (Option.some @@ Row.Label "endorsement", list_ty nat_ty)
         ; Leaf (Option.some @@ Row.Label "proposal", nat_ty)
         ])
  in
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let param = left (Some "endorsement", Some "proposal", param_ty) lst in
  let expr =
    let_in
      (var "PROBLEM")
      ~rhs:(nat 0)
      ~in_:
        (let_in
           (var "_ignore")
           ~rhs:
             (if_left
                param
                ~left:
                  { lam_var = var "left_", list_ty nat_ty
                  ; body =
                    fold_left
                      (variable (var "left_") (list_ty nat_ty))
                      ~init:(int 0)
                      ~fold:{ lam_var = var "acc", mk_tuple_ty [ int_ty; int_ty ]; body = 
                        variable (var "PROBLEM") nat_ty
                      }
                  }
                ~right:
                  { lam_var = var "right_", nat_ty
                  ; body = variable (var "PROBLEM") nat_ty
                  })
           ~in_:unit) 
  in
  test_and_print "last used var in fold_left body" expr;
  [%expect {|
    --- last used var in fold_left body ---
    Node: Let_in
      luv = {PROBLEM, _ignore, acc, left_, right_} nuv = {}
      let_var=PROBLEM
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {PROBLEM, _ignore, acc, left_, right_} nuv = {_ignore}
        let_var=_ignore
        rhs:
        Node: If_left
          luv = {PROBLEM, acc, left_, right_} nuv = {}
          subject:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Const
                  luv = {} nuv = {}
                Node: Prim
                  luv = {} nuv = {}
                  args:
          if_left lam_var=left_
          Node: Fold_left
            luv = {PROBLEM, acc, left_} nuv = {}
            collection:
            Node: Variable
              luv = {left_} nuv = {}
              var=left_
            init:
            Node: Const
              luv = {} nuv = {}
            lam_var=acc
            body:
            Node: Variable
              luv = {} nuv = {acc}
              var=PROBLEM
          if_right lam_var=right_
          Node: Variable
            luv = {PROBLEM} nuv = {acc, left_, right_}
            var=PROBLEM
        in_:
        Node: Const
          luv = {} nuv = {}

    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      LEFT (or (list %endorsement nat) (nat %proposal)) ;
      IF_LEFT
        { PUSH int 0 ; SWAP ; ITER { SWAP ; PAIR ; DROP ; DUP 1 } ; SWAP ; DROP }
        { DROP } ;
      DROP ;
      UNIT }

    Optimised:
    { UNIT } |}]

let%expect_test "last used var in fold_right body" =
  let param_ty =
    or_ty
      (Node
         [ Leaf (Option.some @@ Row.Label "endorsement", list_ty nat_ty)
         ; Leaf (Option.some @@ Row.Label "proposal", nat_ty)
         ])
  in
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let param = left (Some "endorsement", Some "proposal", param_ty) lst in
  let expr =
    let_in
      (var "PROBLEM")
      ~rhs:(nat 0)
      ~in_:
        (let_in
           (var "_ignore")
           ~rhs:
             (if_left
                param
                ~left:
                  { lam_var = var "left_", list_ty nat_ty
                  ; body =
                    fold_right
                      (variable (var "left_") (list_ty nat_ty))
                      ~init:(int 0)
                      ~fold:{ lam_var = var "acc", mk_tuple_ty [ int_ty; int_ty ]; body = 
                        variable (var "PROBLEM") nat_ty
                      }
                  }
                ~right:
                  { lam_var = var "right_", nat_ty
                  ; body = variable (var "PROBLEM") nat_ty
                  })
           ~in_:unit) 
  in
  test_and_print "last used var in fold_right body" expr;
  [%expect {|
    --- last used var in fold_right body ---
    Node: Let_in
      luv = {PROBLEM, _ignore, acc, left_, right_} nuv = {}
      let_var=PROBLEM
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {PROBLEM, _ignore, acc, left_, right_} nuv = {_ignore}
        let_var=_ignore
        rhs:
        Node: If_left
          luv = {PROBLEM, acc, left_, right_} nuv = {}
          subject:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Const
                  luv = {} nuv = {}
                Node: Prim
                  luv = {} nuv = {}
                  args:
          if_left lam_var=left_
          Node: Fold_right
            luv = {PROBLEM, acc, left_} nuv = {}
            collection:
            Node: Variable
              luv = {left_} nuv = {}
              var=left_
            init:
            Node: Const
              luv = {} nuv = {}
            lam_var=acc
            body:
            Node: Variable
              luv = {} nuv = {acc}
              var=PROBLEM
          if_right lam_var=right_
          Node: Variable
            luv = {PROBLEM} nuv = {acc, left_, right_}
            var=PROBLEM
        in_:
        Node: Const
          luv = {} nuv = {}

    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      LEFT (or (list %endorsement nat) (nat %proposal)) ;
      IF_LEFT
        { PUSH int 0 ;
          SWAP ;
          NIL nat ;
          SWAP ;
          ITER { CONS } ;
          ITER { PAIR ; DROP ; DUP 1 } ;
          SWAP ;
          DROP }
        { DROP } ;
      DROP ;
      UNIT }

    Optimised:
    { UNIT } |}]

let%expect_test "last used var in while body" =
  let param_ty =
    or_ty
      (Node
         [ Leaf (Option.some @@ Row.Label "endorsement", list_ty nat_ty)
         ; Leaf (Option.some @@ Row.Label "proposal", nat_ty)
         ])
  in
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let param = left (Some "endorsement", Some "proposal", param_ty) lst in
  let expr =
    let_in
      (var "PROBLEM")
      ~rhs:(nat 0)
      ~in_:
        (let_in
           (var "_ignore")
           ~rhs:
             (if_left
                param
                ~left:
                  { lam_var = var "left_", list_ty nat_ty
                  ; body =
                    while_
                      (lt (variable (var "left_") (list_ty nat_ty)) (int 10))
                      ~body:(variable (var "PROBLEM") nat_ty)
                  }
                ~right:
                  { lam_var = var "right_", nat_ty
                  ; body = variable (var "PROBLEM") nat_ty
                  })
           ~in_:unit) 
  in
  test_and_print "last used var in while body" expr;
  [%expect {|
    --- last used var in while body ---
    Node: Let_in
      luv = {PROBLEM, _ignore, left_, right_} nuv = {}
      let_var=PROBLEM
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {PROBLEM, _ignore, left_, right_} nuv = {_ignore}
        let_var=_ignore
        rhs:
        Node: If_left
          luv = {PROBLEM, left_, right_} nuv = {}
          subject:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Const
                  luv = {} nuv = {}
                Node: Prim
                  luv = {} nuv = {}
                  args:
          if_left lam_var=left_
          Node: While
            luv = {PROBLEM, left_} nuv = {}
            cond:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Variable
                  luv = {} nuv = {}
                  var=left_
                Node: Const
                  luv = {} nuv = {}
            body:
            Node: Variable
              luv = {} nuv = {}
              var=PROBLEM
          if_right lam_var=right_
          Node: Variable
            luv = {PROBLEM} nuv = {left_, right_}
            var=PROBLEM
        in_:
        Node: Const
          luv = {} nuv = {}

    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      LEFT (or (list %endorsement nat) (nat %proposal)) ;
      IF_LEFT
        { PUSH int 10 ;
          DUP 2 ;
          COMPARE ;
          LT ;
          LOOP { DUP 2 ; DROP ; PUSH int 10 ; DUP 2 ; COMPARE ; LT } ;
          SWAP ;
          DROP ;
          DROP ;
          UNIT }
        { DROP } ;
      DROP ;
      UNIT }

    Optimised:
    { PUSH (list nat) { 1 ; 2 } ;
      LEFT (or (list nat) nat) ;
      IF_LEFT
        { PUSH int 10 ;
          DUP 2 ;
          COMPARE ;
          LT ;
          LOOP { PUSH int 10 ; DUP 2 ; COMPARE ; LT } ;
          DROP }
        { DROP } ;
      UNIT } |}]

let%expect_test "last used var in while_left body" =
  let param_ty =
    or_ty
      (Node
         [ Leaf (Option.some @@ Row.Label "endorsement", list_ty nat_ty)
         ; Leaf (Option.some @@ Row.Label "proposal", nat_ty)
         ])
  in
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let param = left (Some "endorsement", Some "proposal", param_ty) lst in
  let expr =
    let_in
      (var "PROBLEM")
      ~rhs:(nat 0)
      ~in_:
        (let_in
           (var "_ignore")
           ~rhs:
             (if_left
                param
                ~left:
                  { lam_var = var "left_", list_ty nat_ty
                  ; body =
                    while_left
                      param
                      ~body:{ lam_var = var "_iter", nat_ty; body = (
                        let_in (var "_ignore2") 
                        ~rhs:(variable (var "PROBLEM") nat_ty) 
                        ~in_:param) }
                  }
                ~right:
                  { lam_var = var "right_", nat_ty
                  ; body = variable (var "PROBLEM") nat_ty
                  })
           ~in_:unit) 
  in
  test_and_print "last used var in while_left body" expr;
  [%expect {|
    --- last used var in while_left body ---
    Node: Let_in
      luv = {PROBLEM, _ignore, _ignore2, _iter, left_, right_} nuv = {}
      let_var=PROBLEM
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {PROBLEM, _ignore, _ignore2, _iter, left_, right_} nuv = {_ignore}
        let_var=_ignore
        rhs:
        Node: If_left
          luv = {PROBLEM, _ignore2, _iter, left_, right_} nuv = {}
          subject:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Const
                  luv = {} nuv = {}
                Node: Prim
                  luv = {} nuv = {}
                  args:
          if_left lam_var=left_
          Node: While_left
            luv = {PROBLEM, _ignore2, _iter} nuv = {left_}
            cond:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Const
                  luv = {} nuv = {}
                Node: Prim
                  luv = {} nuv = {}
                  args:
                  Node: Const
                    luv = {} nuv = {}
                  Node: Prim
                    luv = {} nuv = {}
                    args:
            lam_var=_iter
            body:
            Node: Let_in
              luv = {_ignore2} nuv = {_ignore2, _iter}
              let_var=_ignore2
              rhs:
              Node: Variable
                luv = {} nuv = {}
                var=PROBLEM
              in_:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Prim
                  luv = {} nuv = {}
                  args:
                  Node: Const
                    luv = {} nuv = {}
                  Node: Prim
                    luv = {} nuv = {}
                    args:
                    Node: Const
                      luv = {} nuv = {}
                    Node: Prim
                      luv = {} nuv = {}
                      args:
          if_right lam_var=right_
          Node: Variable
            luv = {PROBLEM} nuv = {_ignore2, _iter, right_}
            var=PROBLEM
        in_:
        Node: Const
          luv = {} nuv = {}

    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      LEFT (or (list %endorsement nat) (nat %proposal)) ;
      IF_LEFT
        { DROP ;
          NIL nat ;
          PUSH nat 2 ;
          CONS ;
          PUSH nat 1 ;
          CONS ;
          LEFT (or (list %endorsement nat) (nat %proposal)) ;
          LEFT (or (list %endorsement nat) (nat %proposal)) ;
          LOOP_LEFT
            { DROP ;
              DUP 1 ;
              DROP ;
              NIL nat ;
              PUSH nat 2 ;
              CONS ;
              PUSH nat 1 ;
              CONS ;
              LEFT (or (list %endorsement nat) (nat %proposal)) } ;
          SWAP ;
          DROP }
        { DROP } ;
      DROP ;
      UNIT }

    Optimised:
    { PUSH nat 0 ;
      PUSH (list nat) { 1 ; 2 } ;
      LEFT (or (list nat) nat) ;
      IF_LEFT
        { DROP ;
          PUSH (list nat) { 1 ; 2 } ;
          LEFT (or (list nat) nat) ;
          LEFT (or (list nat) nat) ;
          LOOP_LEFT { DROP ; PUSH (list nat) { 1 ; 2 } ; LEFT (or (list nat) nat) } ;
          DROP 2 }
        { DROP 2 } ;
      UNIT } |}]

let%expect_test "last used var in for body" =
  let param_ty =
    or_ty
      (Node
         [ Leaf (Option.some @@ Row.Label "endorsement", list_ty nat_ty)
         ; Leaf (Option.some @@ Row.Label "proposal", nat_ty)
         ])
  in
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let param = left (Some "endorsement", Some "proposal", param_ty) lst in
  let expr =
    let_in
      (var "PROBLEM")
      ~rhs:(nat 0)
      ~in_:
        (let_in
           (var "_ignore")
           ~rhs:
             (if_left
                param
                ~left:
                  { lam_var = var "left_", list_ty nat_ty
                  ; body =
                    (for_
                      (mut_var "idx")
                      ~init:(int 0)
                      ~cond:(lt (deref (mut_var "idx") int_ty) (int 5))
                      ~update:(assign (mut_var "idx") (add (deref (mut_var "idx") int_ty) (int 1)))
                      ~body:(variable (var "PROBLEM") nat_ty) )
                  }
                ~right:
                  { lam_var = var "right_", nat_ty
                  ; body = variable (var "PROBLEM") nat_ty
                  })
           ~in_:unit) 
  in
  test_and_print "last used var in for body" expr;
  [%expect {|
    --- last used var in for body ---
    Node: Let_in
      luv = {PROBLEM, _ignore, idx, left_, right_} nuv = {}
      let_var=PROBLEM
      rhs:
      Node: Const
        luv = {} nuv = {}
      in_:
      Node: Let_in
        luv = {PROBLEM, _ignore, idx, left_, right_} nuv = {_ignore}
        let_var=_ignore
        rhs:
        Node: If_left
          luv = {PROBLEM, idx, left_, right_} nuv = {}
          subject:
          Node: Prim
            luv = {} nuv = {}
            args:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Const
                luv = {} nuv = {}
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Const
                  luv = {} nuv = {}
                Node: Prim
                  luv = {} nuv = {}
                  args:
          if_left lam_var=left_
          Node: For
            luv = {PROBLEM, idx} nuv = {left_}
            index=idx
            init:
            Node: Const
              luv = {} nuv = {}
            cond:
            Node: Prim
              luv = {} nuv = {}
              args:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Deref
                  luv = {} nuv = {}
                  mut_var=idx
                Node: Const
                  luv = {} nuv = {}
            update:
            Node: Assign
              luv = {} nuv = {}
              let_var=idx
              value:
              Node: Prim
                luv = {} nuv = {}
                args:
                Node: Deref
                  luv = {} nuv = {}
                  mut_var=idx
                Node: Const
                  luv = {} nuv = {}
            body:
            Node: Variable
              luv = {} nuv = {}
              var=PROBLEM
          if_right lam_var=right_
          Node: Variable
            luv = {PROBLEM} nuv = {idx, right_}
            var=PROBLEM
        in_:
        Node: Const
          luv = {} nuv = {}

    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      LEFT (or (list %endorsement nat) (nat %proposal)) ;
      IF_LEFT
        { DROP ;
          PUSH int 0 ;
          PUSH int 5 ;
          DUP 2 ;
          COMPARE ;
          LT ;
          LOOP { DUP 2 ;
                 DROP ;
                 PUSH int 1 ;
                 DUP 2 ;
                 ADD ;
                 DUG 1 ;
                 DIG 0 ;
                 DROP ;
                 UNIT ;
                 DROP ;
                 PUSH int 5 ;
                 DUP 2 ;
                 COMPARE ;
                 LT } ;
          DROP ;
          DROP ;
          UNIT }
        { DROP } ;
      DROP ;
      UNIT }

    Optimised:
    { PUSH (list nat) { 1 ; 2 } ;
      LEFT (or (list nat) nat) ;
      IF_LEFT
        { DROP ;
          PUSH int 0 ;
          PUSH int 5 ;
          DUP 2 ;
          COMPARE ;
          LT ;
          LOOP { PUSH int 1 ; ADD ; PUSH int 5 ; DUP 2 ; COMPARE ; LT } ;
          DROP }
        { DROP } ;
      UNIT } |}]
