open Core
open Lltz_ir.Dsl

module LM = Lltz_michelson 

let empty_stack : LM.Stack.t = []

let compile_and_collect_instructions expr =
  let compiled_instruction = LM.compile expr in
  (compiled_instruction empty_stack).instructions

let print_instructions instructions =
  List.iter ~f:(fun instruction ->
    Michelson.Ast.pp Format.std_formatter instruction;
    Format.print_newline ()
  ) instructions;
  Format.print_flush ()

let test_expr expr =
  let instructions = compile_and_collect_instructions expr in
  print_instructions instructions;;

let%expect_test "unit" =
  test_expr (unit ());
  [%expect {| (PUSH unit UNIT) |}]
;;

let%expect_test "bool true" =
  test_expr (bool true);
  [%expect {| (PUSH bool True) |}]
;;

let%expect_test "nat 42" =
  test_expr (nat 42);
  [%expect {| (PUSH nat 42) |}]
;;

let%expect_test "int (-10)" =
  test_expr (int (-10));
  [%expect {| (PUSH int -10) |}]
;;

let%expect_test "mutez 1000" =
  test_expr (mutez 1000);
  [%expect {| (PUSH mutez 1000) |}]
;;

let%expect_test "string \"hello\"" =
  test_expr (string "hello");
  [%expect {| (PUSH string "hello") |}]
;;

let%expect_test "key" =
  test_expr (key "edpk...");
  [%expect {| (PUSH key "edpk...") |}]
;;

let%expect_test "key_hash" =
  test_expr (key_hash "tz1...");
  [%expect {| (PUSH key_hash "tz1...") |}]
;;

let%expect_test "bytes \"0x1234\"" =
  test_expr (bytes "0x1234");
  [%expect {| (PUSH bytes 0x307831323334) |}]
;;

let%expect_test "chain_id" =
  test_expr (chain_id "NetXdQprcVkpaWU");
  [%expect {| (PUSH chain_id "NetXdQprcVkpaWU") |}]
;;

let%expect_test "address" =
  test_expr (address_const "KT1...");
  [%expect {| (PUSH address "KT1...") |}]
;;

let%expect_test "timestamp" =
  test_expr (timestamp "2021-07-01T00:00:00Z");
  [%expect {| (PUSH timestamp "2021-07-01T00:00:00Z") |}]
;;

let%expect_test "bls12_381_g1" =
  test_expr (bls12_381_g1 "BLS12_381_G1...");
  [%expect {| (PUSH bls12_381_g1 "BLS12_381_G1...") |}]
;;

let%expect_test "bls12_381_g2" =
  test_expr (bls12_381_g2 "BLS12_381_G2...");
  [%expect {| (PUSH bls12_381_g2 "BLS12_381_G2...") |}]
;;

let%expect_test "bls12_381_fr" =
  test_expr (bls12_381_fr "BLS12_381_FR...");
  [%expect {| (PUSH bls12_381_fr "BLS12_381_FR...") |}]
;;

let%expect_test "signature" =
  test_expr (signature "sig...");
  [%expect {| (PUSH signature "sig...") |}]
;;

let%expect_test "variable x" =
  test_expr (let_in (var "x") ~rhs:(nat 1) ~in_:(variable (var "x")));
  [%expect {|
    (PUSH nat 1)
    (DUP 1)
    SWAP
    DROP |}]
;;

let%expect_test "let_in" =
  test_expr (let_in (var "x") ~rhs:(nat 1) ~in_:(variable (var "x")));
  [%expect {|
    (PUSH nat 1)
    (DUP 1)
    SWAP
    DROP |}]
;;

let%expect_test "lambda" =
  test_expr (lambda (var "x",nat_ty ()) ~return_type:(nat_ty ()) ~body:(variable (var "x")));
  [%expect {| (LAMBDA nat nat { DUP 1 ; SWAP ; DROP }) |}]
;;

let%expect_test "lambda_rec" =
  test_expr (lambda_rec (var "x",nat_ty ()) ~mu:(var "y") ~return_type:(nat_ty ()) ~body:(variable (var "x")));
  [%expect {| (LAMBDA_REC nat nat { DUP 1 ; SWAP ; DROP ; SWAP ; DROP }) |}]
;;

let%expect_test "app" =
  test_expr (app (lambda (var "x",nat_ty ()) ~return_type:(nat_ty ()) ~body:(variable (var "x"))) (nat 1));
  [%expect {|
    ("Stack before" (trace_point 2) (stack ()))
    ("Stack before" (trace_point 1) (stack ()))
    ("Stack after" (trace_point 1) (stack (Value)))
    ("Stack before" (trace_point 0) (stack (Value)))
    ("Stack after" (trace_point 0) (stack (Value Value)))
    ("Stack after" (trace_point 2) (stack (Value)))
    (LAMBDA nat nat { DUP 1 ; SWAP ; DROP })
    (PUSH nat 1)
    EXEC |}]
;;

let%expect_test "let_mut_in" =
  test_expr (let_mut_in (mut_var "x") ~rhs:(nat 1) ~in_:(deref (mut_var "x")));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure lib/lltz_michelson/lltz_michelson.ml:223:59"
  Raised at Lltz_michelson.compile in file "lib/lltz_michelson/lltz_michelson.ml", line 223, characters 59-71
  Called from Test_dsl.compile_and_collect_instructions in file "test/test_dsl.ml", line 9, characters 29-44
  Called from Test_dsl.test_expr in file "test/test_dsl.ml" (inlined), line 20, characters 21-58
  Called from Test_dsl.(fun) in file "test/test_dsl.ml", line 146, characters 2-78
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "deref" =
  test_expr (let_mut_in (mut_var "x") ~rhs:(nat 1) ~in_:(deref (mut_var "x")));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure lib/lltz_michelson/lltz_michelson.ml:223:59"
  Raised at Lltz_michelson.compile in file "lib/lltz_michelson/lltz_michelson.ml", line 223, characters 59-71
  Called from Test_dsl.compile_and_collect_instructions in file "test/test_dsl.ml", line 9, characters 29-44
  Called from Test_dsl.test_expr in file "test/test_dsl.ml" (inlined), line 20, characters 21-58
  Called from Test_dsl.(fun) in file "test/test_dsl.ml", line 162, characters 2-78
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "assign" =
  test_expr (let_mut_in (mut_var "x") ~rhs:(nat 1) ~in_:(assign (mut_var "x") (nat 7)));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure lib/lltz_michelson/lltz_michelson.ml:223:59"
  Raised at Lltz_michelson.compile in file "lib/lltz_michelson/lltz_michelson.ml", line 223, characters 59-71
  Called from Test_dsl.compile_and_collect_instructions in file "test/test_dsl.ml", line 9, characters 29-44
  Called from Test_dsl.test_expr in file "test/test_dsl.ml" (inlined), line 20, characters 21-58
  Called from Test_dsl.(fun) in file "test/test_dsl.ml", line 178, characters 2-87
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "if_bool" =
  test_expr (if_bool(bool true) ~then_:(nat 1) ~else_:(nat 0));
  [%expect {|
    (PUSH bool True)
    (IF { PUSH nat 1 } { PUSH nat 0 }) |}]
;;

let%expect_test "if_none" =
  test_expr (if_none (none (nat_ty ())) ~none:(nat 0) ~some:(var "x", variable (var "x")));
  [%expect {|
    (NONE nat)
    (IF_NONE { PUSH nat 0 } { DUP 1 ; SWAP ; DROP }) |}]
;;

let%expect_test "if_cons" =
  test_expr (if_cons (nil (nat_ty ())) ~empty:(nat 0) ~nonempty:(var "x", var "y", variable (var "x")));
  [%expect {|
    ("Stack before" (trace_point 3) (stack ()))
    ("Stack after" (trace_point 3) (stack (Value)))
    (NIL nat)
    (IF_CONS { PUSH nat 0 } { DUP 1 ; SWAP ; DROP ; SWAP ; DROP }) |}]
;;

let%expect_test "if_left" =
  test_expr (if_left (left (None,None,nat_ty ()) (nat 5)) ~left:(var "x", variable (var "x")) ~right:(var "y", variable (var "y")));
  [%expect {|
    (PUSH nat 5)
    (LEFT nat)
    (IF_LEFT { DUP 1 ; SWAP ; DROP } { DUP 1 ; SWAP ; DROP }) |}]
;;

let%expect_test "while_" =
  test_expr (while_ (bool true) ~body:(nat 1));
  [%expect {|
    (PUSH bool True)
    (LOOP { PUSH nat 1 ; PUSH bool True }) |}]
;;

let%expect_test "while_left" =
  test_expr (while_left (left (None,None, nat_ty ()) (nat 5)) ~body:(nat 1));
  [%expect {|
    (PUSH nat 5)
    (LEFT nat)
    (LOOP_LEFT { PUSH nat 1 ; PUSH nat 5 ; LEFT nat }) |}]
;;

let%expect_test "for_" =
  test_expr (for_ (mut_var "i") ~init:(nat 0) ~invariant:(nat 10) ~variant:(nat 1) ~body:(nat 1));
  [%expect {|
    (PUSH nat 0)
    (PUSH nat 10)
    (LOOP { PUSH nat 1 ; SWAP ; DROP ; PUSH nat 1 ; PUSH nat 10 })
    DROP |}]
;;

let%expect_test "for_each" =
  test_expr (for_each [var "x"] ~of_:(nil (nat_ty ())) ~body:(variable (var "x")));
  [%expect {|
    (NIL nat)
    (ITER { DUP 1 ; SWAP ; DROP }) |}]
;;

let%expect_test "map" =
  test_expr (map (nil (nat_ty ())) ~map:([var "x"], variable (var "x")));
  [%expect {|
    (NIL nat)
    (MAP { DUP 1 ; SWAP ; DROP }) |}]
;;

let%expect_test "fold_left" =
  test_expr (fold_left (nil (nat_ty ())) ~init:(var "y", nat 0) ~fold:(var "x", variable (var "x")));
  [%expect {|
    (PUSH nat 0)
    (NIL nat)
    (ITER { DUP 1 ; SWAP ; DROP ; SWAP ; DROP ; DROP }) |}]
;;

let%expect_test "fold_right" =
  test_expr (fold_right (nil (nat_ty ())) ~init:(var "y", nat 0) ~fold:(var "x", variable (var "x")));
  [%expect {|
    (PUSH nat 0)
    (NIL nat)
    (ITER { DUP 1 ; SWAP ; DROP ; SWAP ; DROP ; DROP }) |}]
;;

let%expect_test "let_tuple_in" =
  test_expr (let_tuple_in [var "x"] ~rhs:(tuple (convert_list [nat 1])) ~in_:(variable (var "x")));
  [%expect {|
    ("Stack before" (trace_point 4) (stack (Value)))
    ("Stack after" (trace_point 4) (stack (Value)))
    (PUSH nat 1)
    (DUP 1)
    SWAP
    DROP |}]
;;

let%expect_test "tuple" =
  test_expr (tuple (convert_list [nat 1; nat 2]));
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 2)
    PAIR |}]
;;

let%expect_test "proj" =
  test_expr (proj (tuple (convert_list [nat 1; nat 2])) ~path: (Here [0]));
  [%expect {|
    ("Stack before" (trace_point 6) (stack ()))
    ("Stack before" (trace_point 5) (stack (Value)))
    ("Stack after" (trace_point 5) (stack (Value)))
    ("Stack after" (trace_point 6) (stack (Value)))
    (PUSH nat 1)
    (PUSH nat 2)
    PAIR
    (GET 1)
    (DIP {}) |}]
;;

let%expect_test "update" =
  test_expr (update_tuple (tuple (convert_list [nat 1])) ~component:(Here [0] ) ~update:(nat 2));
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 2)
    SWAP
    DROP |}]
;;

let%expect_test "inj" =
  test_expr (inj (Node ([],Leaf (None,nat_ty ()),[])) (nat 1));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure lib/lltz_michelson/type.ml:15:20"
  Raised at Lltz_michelson__Type.ors.loop in file "lib/lltz_michelson/type.ml", line 15, characters 20-32
  Called from Lltz_michelson.compile_inj in file "lib/lltz_michelson/lltz_michelson.ml", line 508, characters 19-76
  Called from Lltz_michelson.compile in file "lib/lltz_michelson/lltz_michelson.ml", line 249, characters 29-50
  Called from Test_dsl.compile_and_collect_instructions in file "test/test_dsl.ml", line 9, characters 29-44
  Called from Test_dsl.test_expr in file "test/test_dsl.ml" (inlined), line 20, characters 21-58
  Called from Test_dsl.(fun) in file "test/test_dsl.ml", line 321, characters 2-62
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "match_" =
  test_expr (
    match_ (inj (Node ([Leaf (None,nat_ty ())],Leaf (None,nat_ty ()),[Leaf (None,nat_ty ())])) (nat 1)) 
    ~cases:(
      Node [
        Leaf (None, ((var "x", nat_ty ()), nat_ty (), add (variable (var "x")) (nat 1)));
        Leaf (None, ((var "y", nat_ty ()), nat_ty (), add (variable (var "y")) (nat 2)));
        Leaf (None, ((var "z", nat_ty ()), nat_ty (), add (variable (var "z")) (nat 3)))
      ]));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure lib/lltz_michelson/type.ml:15:20"
  Raised at Lltz_michelson__Type.ors.loop in file "lib/lltz_michelson/type.ml", line 15, characters 20-32
  Called from Lltz_michelson.compile_inj in file "lib/lltz_michelson/lltz_michelson.ml", line 508, characters 19-76
  Called from Lltz_michelson.compile in file "lib/lltz_michelson/lltz_michelson.ml", line 249, characters 29-50
  Called from Lltz_michelson.compile_match in file "lib/lltz_michelson/lltz_michelson.ml", line 532, characters 22-37
  Called from Lltz_michelson.compile in file "lib/lltz_michelson/lltz_michelson.ml", line 250, characters 35-62
  Called from Test_dsl.compile_and_collect_instructions in file "test/test_dsl.ml", line 9, characters 29-44
  Called from Test_dsl.test_expr in file "test/test_dsl.ml" (inlined), line 20, characters 21-58
  Called from Test_dsl.(fun) in file "test/test_dsl.ml", line 339, characters 2-423
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "create_contract" =
  test_expr (create_contract () ~storage:(nat_ty ()) ~parameter:(nat_ty ()) ~code:(nat 1) ~delegate:(unit ()) ~initial_balance:(mutez 1000) ~initial_storage:(unit ()));
  [%expect {|
    (PUSH unit UNIT)
    (PUSH mutez 1000)
    (PUSH unit UNIT)
    (CREATE_CONTRACT nat nat { PUSH nat 1 }) |}]
;;

let%expect_test "amount" =
  test_expr (amount ());
  [%expect {| AMOUNT |}]
;;

let%expect_test "balance" =
  test_expr (balance ());
  [%expect {| BALANCE |}]
;;

let%expect_test "chain_id_prim" =
  test_expr (chain_id_prim ());
  [%expect {| CHAIN_ID |}]
;;

let%expect_test "level" =
  test_expr (level ());
  [%expect {| LEVEL |}]
;;

let%expect_test "now" =
  test_expr (now ());
  [%expect {| NOW |}]
;;

let%expect_test "self" =
  test_expr (self None);
  [%expect {| SELF |}]

  let%expect_test "self_address" =
  test_expr (self_address ());
  [%expect {| SELF_ADDRESS |}]
;;

let%expect_test "sender" =
  test_expr (sender ());
  [%expect {| SENDER |}]
;;

let%expect_test "source" =
  test_expr (source ());
  [%expect {| SOURCE |}]
;;

let%expect_test "total_voting_power" =
  test_expr (total_voting_power ());
  [%expect {| TOTAL_VOTING_POWER |}]
;;

let%expect_test "empty_bigmap" =
  test_expr (empty_bigmap (nat_ty ()) (nat_ty ()));
  [%expect {| (EMPTY_BIG_MAP nat nat) |}]
;;

let%expect_test "empty_map" =
  test_expr (empty_map (nat_ty ()) (nat_ty ()));
  [%expect {| (EMPTY_MAP nat nat) |}]
;;

let%expect_test "empty_set" =
  test_expr (empty_set (nat_ty ()));
  [%expect {| (EMPTY_SET nat) |}]
;;

let%expect_test "nil" =
  test_expr (nil (nat_ty ()));
  [%expect {| (NIL nat) |}]
;;

let%expect_test "none" =
  test_expr (none (nat_ty ()));
  [%expect {| (NONE nat) |}]
;;

let%expect_test "sapling_empty_state" =
  test_expr (sapling_empty_state 0);
  [%expect {| (SAPLING_EMPTY_STATE 0) |}]
;;

let%expect_test "unit_prim" =
  test_expr (unit_prim ());
  [%expect {| UNIT |}]
;;

let%expect_test "car" =
  test_expr (car (pair (None, None) (nat 5) (string "test")));
  [%expect {|
    (PUSH nat 5)
    (PUSH string "test")
    PAIR
    CAR |}]
;;

let%expect_test "cdr" =
  test_expr (cdr (pair (None, None) (nat 5) (string "test")));
  [%expect {|
    (PUSH nat 5)
    (PUSH string "test")
    PAIR
    CDR |}]
;;

let%expect_test "left" =
  test_expr (left (Some "opt1", Some "opt2",nat_ty ()) (nat 5));
  [%expect {|
    (PUSH nat 5)
    (LEFT nat) |}]
;;

let%expect_test "right" =
  test_expr (right (Some "opt1", Some "opt2",nat_ty ()) (nat 5));
  [%expect {|
    (PUSH nat 5)
    (RIGHT nat) |}]
;;

let%expect_test "some" =
  test_expr (some (nat 1));
  [%expect {|
    (PUSH nat 1)
    SOME |}]
;;

let%expect_test "eq" =
  test_expr (eq (nat 42) (nat 43));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 43)
    EQ |}]
;;

let%expect_test "abs" =
  test_expr (abs (int (-10)));
  [%expect {|
    (PUSH int -10)
    ABS |}]
;;

let%expect_test "neg" =
  test_expr (neg (int 10));
  [%expect {|
    (PUSH int 10)
    NEG |}]
;;

let%expect_test "nat_prim" =
  test_expr (nat_prim (int 10));
  [%expect {|
    (PUSH int 10)
    INT |}]
;;

let%expect_test "int_prim" =
  test_expr (int_prim (nat 42));
  [%expect {|
    (PUSH nat 42)
    INT |}]
;;

let%expect_test "bytes_prim" =
  test_expr (bytes_prim (bytes "0x1234"));
  [%expect {|
    (PUSH bytes 0x307831323334)
    PACK |}]
;;

let%expect_test "is_nat" =
  test_expr (is_nat (int (-10)));
  [%expect {|
    (PUSH int -10)
    ISNAT |}]
;;

let%expect_test "neq" =
  test_expr (neq (nat 42) (nat 43));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 43)
    NEQ |}]
;;

let%expect_test "le" =
  test_expr (le (nat 42) (nat 43));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 43)
    LE |}]
;;

let%expect_test "lt" =
  test_expr (lt (nat 42) (nat 43));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 43)
    LT |}]
;;

let%expect_test "ge" =
  test_expr (ge (nat 42) (nat 43));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 43)
    GE |}]
;;

let%expect_test "gt" =
  test_expr (gt (nat 42) (nat 43));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 43)
    GT |}]
;;

let%expect_test "not" =
  test_expr (not (bool true));
  [%expect {|
    (PUSH bool True)
    NOT |}]
;;

let%expect_test "size" =
  test_expr (size (string "test"));
  [%expect {|
    (PUSH string "test")
    SIZE |}]
;;

let%expect_test "address" =
  test_expr (address (contract (None, nat_ty ()) (address_const "KT1...")));
  [%expect {|
    (PUSH address "KT1...")
    (CONTRACT nat)
    ADDRESS |}]
;;

let%expect_test "implicit_account" =
  test_expr (implicit_account (key_hash "tz1..."));
  [%expect {|
    (PUSH key_hash "tz1...")
    IMPLICIT_ACCOUNT |}]
;;

let%expect_test "contract" =
  test_expr (contract (None, nat_ty ()) (address_const "KT1..."));
  [%expect {|
    (PUSH address "KT1...")
    (CONTRACT nat) |}]
;;

let%expect_test "pack" =
  test_expr (pack (string "test"));
  [%expect {|
    (PUSH string "test")
    PACK |}]
;;

let%expect_test "unpack" =
  test_expr (unpack (nat_ty ()) (bytes "0x1234"));
  [%expect {|
    (PUSH bytes 0x307831323334)
    (UNPACK nat) |}]
;;

let%expect_test "hash_key" =
  test_expr (hash_key (key "edpk..."));
  [%expect {|
    (PUSH key "edpk...")
    HASH_KEY |}]
;;

let%expect_test "blake2b" =
  test_expr (blake2b (bytes "0x1234"));
  [%expect {|
    (PUSH bytes 0x307831323334)
    BLAKE2B |}]
;;

let%expect_test "sha256" =
  test_expr (sha256 (bytes "0x1234"));
  [%expect {|
    (PUSH bytes 0x307831323334)
    SHA256 |}]
;;

let%expect_test "sha512" =
  test_expr (sha512 (bytes "0x1234"));
  [%expect {|
    (PUSH bytes 0x307831323334)
    SHA512 |}]
;;

let%expect_test "keccak" =
  test_expr (keccak (bytes "0x1234"));
  [%expect {|
    (PUSH bytes 0x307831323334)
    KECCAK |}]
;;

let%expect_test "sha3" =
  test_expr (sha3 (bytes "0x1234"));
  [%expect {|
    (PUSH bytes 0x307831323334)
    SHA3 |}]
;;

let%expect_test "set_delegate" =
  test_expr (set_delegate (some (key_hash "tz1...")));
  [%expect {|
    (PUSH key_hash "tz1...")
    SOME
    SET_DELEGATE |}]
;;

let%expect_test "read_ticket" =
  test_expr (read_ticket (ticket (string "Ticket_Content") (nat 1)));
  [%expect {|
    (PUSH string "Ticket_Content")
    (PUSH nat 1)
    TICKET
    READ_TICKET |}]
;;

let%expect_test "join_tickets" =
  test_expr (join_tickets (ticket (string "Ticket_Content") (nat 1)) (ticket (string "Ticket_Content") (nat 1)));
  [%expect {|
    (PUSH string "Ticket_Content")
    (PUSH nat 1)
    TICKET
    (PUSH string "Ticket_Content")
    (PUSH nat 1)
    TICKET
    JOIN_TICKETS |}]
;;

let%expect_test "pairing_check" =
  test_expr (pairing_check (cons (pair ((Some "BLS12_381_G1..."),(Some "BLS12_381_G1...")) (string "test1") (string "test2")) (nil (string_ty ()))));
  [%expect {|
    (PUSH string "test1")
    (PUSH string "test2")
    PAIR
    (NIL string)
    CONS
    PAIRING_CHECK |}]
;;

let%expect_test "voting_power" =
  test_expr (voting_power (key_hash "tz1..."));
  [%expect {|
    (PUSH key_hash "tz1...")
    VOTING_POWER |}]
;;

let%expect_test "getn" =
  test_expr (getn 0 (pair (None, None) (nat 1) (nat 2)));
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 2)
    PAIR
    (GET 0) |}]
;;

let%expect_test "cast" =
  test_expr (cast (nat_ty ()) (int 42));
  
  [%expect {|
    (PUSH int 42)
    (CAST nat) |}]
;;

let%expect_test "rename" = assert false
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure test/test_dsl.ml:743:27"
  Raised at Test_dsl.(fun) in file "test/test_dsl.ml", line 743, characters 27-39
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}];;

let%expect_test "emit" =
  test_expr (emit ((Some "val"), (Some (string_ty ()))) (nat 1));
  [%expect {|
    (PUSH nat 1)
    (EMIT "val" string) |}]
;;

let%expect_test "failwith" =
  test_expr (failwith (string "error"));
  [%expect {|
    (PUSH string "error")
    FAILWITH |}]
;;

let%expect_test "never" =
  test_expr (never (nat 42));
  [%expect {|
    (PUSH nat 42)
    NEVER |}]
;;

let%expect_test "pair" =
  test_expr (pair ((Some "valA"),(Some "valB")) (nat 1) (nat 2));
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 2)
    PAIR |}]
;;

let%expect_test "add" =
  test_expr (add (nat 1) (nat 2));
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 2)
    ADD |}]
;;

let%expect_test "mul" =
  test_expr (mul (nat 2) (nat 3));
  [%expect {|
    (PUSH nat 2)
    (PUSH nat 3)
    MUL |}]
;;

let%expect_test "sub" =
  test_expr (sub (nat 5) (nat 3));
  [%expect {|
    (PUSH nat 5)
    (PUSH nat 3)
    SUB |}]
;;

let%expect_test "sub_mutez" =
  test_expr (sub_mutez (mutez 100) (mutez 50));
  [%expect {|
    (PUSH mutez 100)
    (PUSH mutez 50)
    SUB |}]
;;

let%expect_test "lsr_" =
  test_expr (lsr_ (nat 4) (nat 1));
  [%expect {|
    (PUSH nat 4)
    (PUSH nat 1)
    LSR |}]
;;

let%expect_test "lsl_" =
  test_expr (lsl_ (nat 4) (nat 1));
  [%expect {|
    (PUSH nat 4)
    (PUSH nat 1)
    LSL |}]
;;

let%expect_test "xor" =
  test_expr (xor (nat 3) (nat 2));
  [%expect {|
    (PUSH nat 3)
    (PUSH nat 2)
    XOR |}]
;;

let%expect_test "ediv" =
  test_expr (ediv (nat 9) (nat 3));
  [%expect {|
    (PUSH nat 9)
    (PUSH nat 3)
    EDIV |}]
;;

let%expect_test "and_" =
  test_expr (and_ (nat 3) (nat 1));
  [%expect {|
    (PUSH nat 3)
    (PUSH nat 1)
    AND |}]
;;

let%expect_test "or_" =
  test_expr (or_ (nat 2) (nat 1));
  [%expect {|
    (PUSH nat 2)
    (PUSH nat 1)
    OR |}]
;;

let%expect_test "cons" =
  test_expr (cons (nat 1) (nil (nat_ty ())));
  [%expect {|
    (PUSH nat 1)
    (NIL nat)
    CONS |}]
;;

let%expect_test "compare" =
  test_expr (compare (nat 42) (nat 43));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 43)
    COMPARE |}]
;;

let%expect_test "concat1" =
  test_expr (concat1 (string "hello") (string "world"));
  [%expect {|
    (PUSH string "hello")
    (PUSH string "world")
    CONCAT |}]
;;

let%expect_test "concat2" =
  test_expr (concat2 (bytes "0x12") (bytes "0x34"));
  [%expect {|
    (PUSH bytes 0x30783132)
    (PUSH bytes 0x30783334)
    CONCAT |}]
;;

let%expect_test "get" =
  test_expr (get (nat 42) (empty_map (nat_ty ()) (nat_ty ())));
  [%expect {|
    (PUSH nat 42)
    (EMPTY_MAP nat nat)
    GET |}]
;;

let%expect_test "mem" =
  test_expr (mem (nat 42) (empty_map (nat_ty ()) (nat_ty ())));
  [%expect {|
    (PUSH nat 42)
    (EMPTY_MAP nat nat)
    MEM |}]
;;

let%expect_test "exec" =
  test_expr (exec (lambda (var "x",nat_ty ()) ~return_type:(nat_ty ()) ~body:(add (variable (var "x")) (nat 1))) (nat 1));
  [%expect {|
    (LAMBDA nat nat { DUP 1 ; PUSH nat 1 ; ADD ; SWAP ; DROP })
    (PUSH nat 1)
    EXEC |}]
;;

let%expect_test "apply" =
  test_expr (apply (lambda (var "x",nat_ty ()) ~return_type:(nat_ty ()) ~body:(add (variable (var "x")) (nat 1))) (nat 1));
  [%expect {|
    (LAMBDA nat nat { DUP 1 ; PUSH nat 1 ; ADD ; SWAP ; DROP })
    (PUSH nat 1)
    APPLY |}]
;;

let%expect_test "sapling_verify_update" =
  test_expr (sapling_verify_update (sapling_empty_state 0) (sapling_empty_state 0));
  [%expect {|
    (SAPLING_EMPTY_STATE 0)
    (SAPLING_EMPTY_STATE 0)
    SAPLING_VERIFY_UPDATE |}]
;;

let%expect_test "ticket" =
  test_expr (ticket (string "Ticket_Content") (nat 1));
  [%expect {|
    (PUSH string "Ticket_Content")
    (PUSH nat 1)
    TICKET |}]
;;

let%expect_test "ticket_deprecated" =
  test_expr (ticket_deprecated (string "Ticket_Content") (nat 1));
  [%expect {|
    (PUSH string "Ticket_Content")
    (PUSH nat 1)
    TICKET_DEPRECATED |}]
;;

let%expect_test "split_ticket" =
  test_expr (split_ticket (ticket (string "Ticket_Content") (nat 1)) (pair ((Some "opt1"),(Some "opt2")) (nat 3) (nat 2)));
  [%expect {|
    (PUSH string "Ticket_Content")
    (PUSH nat 1)
    TICKET
    (PUSH nat 3)
    (PUSH nat 2)
    PAIR
    SPLIT_TICKET |}]
;;

let%expect_test "updaten" =
  test_expr (updaten 0 (nat 1) (pair ((Some "opt1"),(Some "opt2")) (nat 3) (nat 2)));
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 3)
    (PUSH nat 2)
    PAIR
    (UPDATE 0) |}]
;;

let%expect_test "view" =
  test_expr (view "test_view" ~return_type:(nat_ty ()) ~d:(nat 1) ~address:(address_const "KT1..."));
  [%expect {|
    (PUSH nat 1)
    (PUSH address "KT1...")
    (VIEW "test_view" nat) |}]
;;

let%expect_test "slice" =
  test_expr (slice (nat 0) ~length:(nat 2) ~seq:(cons (nat 1) (cons (nat 2) (cons (nat 3) (cons (nat 4) (nil (nat_ty ())))))));
  [%expect {|
    (PUSH nat 0)
    (PUSH nat 2)
    (PUSH nat 1)
    (PUSH nat 2)
    (PUSH nat 3)
    (PUSH nat 4)
    (NIL nat)
    CONS
    CONS
    CONS
    CONS
    SLICE |}]
;;

let%expect_test "update" =
  test_expr (update (nat 42) (some (nat 1)) ~of_:(empty_map (nat_ty ()) (nat_ty ())));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 1)
    SOME
    (EMPTY_MAP nat nat)
    UPDATE |}]
;;

let%expect_test "get_and_update" =
  test_expr (get_and_update (nat 42) (some (nat 1)) ~of_:(empty_map (nat_ty ()) (nat_ty ())));
  [%expect {|
    (PUSH nat 42)
    (PUSH nat 1)
    SOME
    (EMPTY_MAP nat nat)
    GET_AND_UPDATE |}]
;;

let%expect_test "transfer_tokens" =
  test_expr (transfer_tokens (nat 1) ~amount:(mutez 100) ~contract:(contract (None,(nat_ty ())) (address_const "KT1...")));
  [%expect {|
    (PUSH nat 1)
    (PUSH mutez 100)
    (PUSH address "KT1...")
    (CONTRACT nat)
    TRANSFER_TOKENS |}]
;;

let%expect_test "check_signature" =
  test_expr (check_signature (key "edpk...") ~signature:(signature "sig...") ~message:(bytes "0x1234"));
  [%expect {|
    (PUSH key "edpk...")
    (PUSH signature "sig...")
    (PUSH bytes 0x307831323334)
    CHECK_SIGNATURE |}]
;;

let%expect_test "open_chest" =
  test_expr (open_chest (string "chest_key") ~chest:(string "chest") ~time:(nat 1));
  [%expect {|
    (PUSH string "chest_key")
    (PUSH string "chest")
    (PUSH nat 1)
    OPEN_CHEST |}]
;;

let%expect_test "nested_let_in" =
  test_expr (
    let_in (var "x") ~rhs:(nat 10) 
      ~in_:(let_in (var "y") ~rhs:(int (-5)) 
      ~in_:(app (lambda (var "z",nat_ty ()) ~return_type:(nat_ty ()) ~body:(mul (variable (var "z")) (nat 2))) (add (variable (var "y")) (variable (var "x")))))
  );
  [%expect {|
    ("Stack before" (trace_point 9) (stack ((Ident y) (Ident x))))
    ("Stack before" (trace_point 8) (stack ((Ident y) (Ident x))))
    ("Stack after" (trace_point 8) (stack (Value (Ident y) (Ident x))))
    ("Stack before" (trace_point 7) (stack (Value (Ident y) (Ident x))))
    ("Stack after" (trace_point 7) (stack (Value Value (Ident y) (Ident x))))
    ("Stack after" (trace_point 9) (stack (Value (Ident y) (Ident x))))
    (PUSH nat 10)
    (PUSH int -5)
    (LAMBDA nat nat { DUP 1 ; PUSH nat 2 ; MUL ; SWAP ; DROP })
    (DUP 2)
    (DUP 4)
    ADD
    EXEC
    SWAP
    DROP
    SWAP
    DROP |}]
;;

let%expect_test "nested_lambda" =
  test_expr (
    lambda (var "x",nat_ty ()) ~return_type:(nat_ty ())
      ~body:(lambda (var "y",nat_ty ()) ~return_type:(nat_ty ()) ~body:(add (variable (var "y")) (nat 2)))
  );
  [%expect {|
    (LAMBDA
       nat
       nat
       { LAMBDA nat nat { DUP 1 ; PUSH nat 2 ; ADD ; SWAP ; DROP } ;
         SWAP ;
         DROP }) |}]
;;

let%expect_test "lambda_in_let" =
  test_expr (
    let_in (var "x") ~rhs:(lambda (var "y",nat_ty ()) ~return_type:(nat_ty ()) ~body:(mul (nat 3) (variable (var "y"))))
      ~in_:(app (variable (var "x")) (nat 5))
  );
  [%expect {|
    ("Stack before" (trace_point 12) (stack ((Ident x))))
    ("Stack before" (trace_point 11) (stack ((Ident x))))
    ("Stack after" (trace_point 11) (stack (Value (Ident x))))
    ("Stack before" (trace_point 10) (stack (Value (Ident x))))
    ("Stack after" (trace_point 10) (stack (Value Value (Ident x))))
    ("Stack after" (trace_point 12) (stack (Value (Ident x))))
    (LAMBDA nat nat { PUSH nat 3 ; DUP 2 ; MUL ; SWAP ; DROP })
    (DUP 1)
    (PUSH nat 5)
    EXEC
    SWAP
    DROP |}]
;;

let%expect_test "nested_if_bool" =
  test_expr (
    if_bool (eq (nat 1) (nat 2)) 
      ~then_:(if_bool (le (nat 1) (nat 10)) 
        ~then_:(nat 1) ~else_:(nat 2)) 
      ~else_:(nat 3)
  );
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 2)
    EQ
    (IF { PUSH nat 1 ; PUSH nat 10 ; LE ; IF { PUSH nat 1 } { PUSH nat 2 } }
        { PUSH nat 3 }) |}]
;;

let%expect_test "if_with_lambda" =
  test_expr (
    if_bool (neq (nat 1) (nat 2)) 
      ~then_:(lambda (var "x",nat_ty ()) ~return_type: (nat_ty ()) ~body:(add (variable (var "x")) (nat 2))) 
      ~else_:(lambda (var "y",nat_ty ()) ~return_type: (nat_ty ()) ~body:(sub (variable (var "y")) (nat 3)))
  );
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 2)
    NEQ
    (IF { LAMBDA nat nat { DUP 1 ; PUSH nat 2 ; ADD ; SWAP ; DROP } }
        { LAMBDA nat nat { DUP 1 ; PUSH nat 3 ; SUB ; SWAP ; DROP } }) |}]
;;

let%expect_test "while_with_complex_body" =
  test_expr (
    while_ (le (nat 0) (nat 10)) 
      ~body:(let_in (var "x") ~rhs:((nat 7)) 
        ~in_:(if_bool (ge (variable (var "x")) (nat 5)) 
          ~then_:(assign (mut_var "x") ((nat 2))) 
          ~else_:(assign (mut_var "x") ((nat 2)))))
  );
  [%expect {|
    ("Stack before" (trace_point 14) (stack ((Ident x))))
    ("Stack before" (trace_point 13) (stack ((Ident x))))
    ("Stack after" (trace_point 13) (stack (Value (Ident x))))
    ("Stack after" (trace_point 14) (stack ((Ident x))))
    ("Stack before" (trace_point 16) (stack ((Ident x))))
    ("Stack before" (trace_point 15) (stack ((Ident x))))
    ("Stack after" (trace_point 15) (stack (Value (Ident x))))
    ("Stack after" (trace_point 16) (stack ((Ident x))))
    (PUSH nat 0)
    (PUSH nat 10)
    LE
    (LOOP { PUSH nat 7 ;
            DUP 1 ;
            PUSH nat 5 ;
            GE ;
            IF { PUSH nat 2 ; DUG 1 ; DIG 0 ; DROP }
               { PUSH nat 2 ; DUG 1 ; DIG 0 ; DROP } ;
            DROP ;
            PUSH nat 0 ;
            PUSH nat 10 ;
            LE }) |}]
;;

let%expect_test "fold_left_with_operations" =
  test_expr (
    fold_left (cons (nat 1) (cons (nat 2) (cons (nat 3) (nil (nat_ty ())))))
      ~init:(var "y", nat 0) 
      ~fold:(var "x", add (variable (var "x")) (variable (var "y")))
  );
  [%expect {|
    (PUSH nat 0)
    (PUSH nat 1)
    (PUSH nat 2)
    (PUSH nat 3)
    (NIL nat)
    CONS
    CONS
    CONS
    (ITER { DUP 1 ; DUP 3 ; ADD ; SWAP ; DROP ; SWAP ; DROP ; DROP }) |}]
;;

let%expect_test "fold_right_with_operations" =
  test_expr (
    fold_right (cons (nat 1) (cons (nat 2) (cons (nat 3) (nil (nat_ty ()))))) 
      ~init:(var "y", nat 0) 
      ~fold:(var "x", sub (variable (var "y")) (variable (var "x")))
  );
  [%expect {|
    (PUSH nat 0)
    (PUSH nat 1)
    (PUSH nat 2)
    (PUSH nat 3)
    (NIL nat)
    CONS
    CONS
    CONS
    (ITER { DUP 2 ; DUP 2 ; SUB ; SWAP ; DROP ; SWAP ; DROP ; DROP }) |}]
;;

let%expect_test "map_with_operations" =
  test_expr (
    map ((cons (nat 1) (cons (nat 2) (cons (nat 3) (nil (nat_ty ())))))) 
      ~map:([var "x"], add (variable (var "x")) (nat 10))
  );
  [%expect {|
    (PUSH nat 1)
    (PUSH nat 2)
    (PUSH nat 3)
    (NIL nat)
    CONS
    CONS
    CONS
    (MAP { DUP 1 ; PUSH nat 10 ; ADD ; SWAP ; DROP }) |}]
;;

let%expect_test "nested_tuple_and_projection" =
  test_expr (
    let_tuple_in [var "x"; var "y"; var "z"] 
      ~rhs:(tuple (convert_list [nat 1; nat 2; nat 3])) 
      ~in_:(let_in (var "a") ~rhs:(proj (tuple (convert_list [variable (var "y"); variable (var "z")])) ~path:(Here [0])) 
        ~in_:(add (variable (var "a")) (variable (var "x"))))
  );
  [%expect {|
    ("Stack before" (trace_point 19) (stack (Value Value Value)))
    ("Stack before" (trace_point 18) (stack ((Ident x) (Ident y) (Ident z))))
    ("Stack before" (trace_point 17)
     (stack (Value (Ident x) (Ident y) (Ident z))))
    ("Stack after" (trace_point 17)
     (stack (Value (Ident x) (Ident y) (Ident z))))
    ("Stack after" (trace_point 18)
     (stack (Value (Ident x) (Ident y) (Ident z))))
    ("Stack after" (trace_point 19) (stack (Value)))
    (PUSH nat 1)
    (PUSH nat 2)
    (PUSH nat 3)
    (PAIR 3)
    (UNPAIR 3)
    (DUP 2)
    (DUP 4)
    PAIR
    (GET 1)
    (DIP {})
    (DUP 1)
    (DUP 3)
    ADD
    SWAP
    DROP
    SWAP
    DROP
    SWAP
    DROP
    SWAP
    DROP |}]
;;

let%expect_test "match_with_single_case" =
  test_expr (
    match_ (left (None,None,nat_ty ()) (nat 1)) 
    ~cases:(
      Node [
        Leaf (None, ((var "x", nat_ty ()), nat_ty (), add (variable (var "x")) (nat 1)));
      ]);
  );
  [%expect {|
    (PUSH nat 1)
    (LEFT nat)
    (IF_LEFT
       { LAMBDA nat nat { DUP 1 ; PUSH nat 1 ; ADD ; SWAP ; DROP } ; EXEC }
       {}) |}]
;;

let%expect_test "create_contract_complex" =
  test_expr (
    create_contract () ~storage:(nat_ty ()) ~parameter:(nat_ty ())
      ~code:(let_in (var "x") ~rhs:(nat 1) 
        ~in_:(add (variable (var "x")) (nat 2))) 
      ~delegate:(nat 42) ~initial_balance:(mutez 1000) ~initial_storage:(nat 10)
  );
  [%expect {|
    (PUSH nat 42)
    (PUSH mutez 1000)
    (PUSH nat 10)
    (CREATE_CONTRACT
       nat
       nat
       { PUSH nat 1 ; DUP 1 ; PUSH nat 2 ; ADD ; SWAP ; DROP }) |}]
;;

let%expect_test "recursive_lambda_usage" =
  test_expr (
    lambda_rec (var "x",nat_ty ()) ~mu:(var "y") 
      ~return_type:(nat_ty ()) 
      ~body:(if_bool (le (variable (var "x")) (nat 0)) 
        ~then_:(nat 1) 
        ~else_:(app (variable (var "y")) (sub (variable (var "x")) (nat 1))))
  );
  [%expect {|
    ("Stack before" (trace_point 22) (stack ((Ident x) (Ident y))))
    ("Stack before" (trace_point 21) (stack ((Ident x) (Ident y))))
    ("Stack after" (trace_point 21) (stack (Value (Ident x) (Ident y))))
    ("Stack before" (trace_point 20) (stack (Value (Ident x) (Ident y))))
    ("Stack after" (trace_point 20) (stack (Value Value (Ident x) (Ident y))))
    ("Stack after" (trace_point 22) (stack (Value (Ident x) (Ident y))))
    (LAMBDA_REC
       nat
       nat
       { DUP 1 ;
         PUSH nat 0 ;
         LE ;
         IF { PUSH nat 1 } { DUP 2 ; DUP 2 ; PUSH nat 1 ; SUB ; EXEC } ;
         SWAP ;
         DROP ;
         SWAP ;
         DROP }) |}]
;;