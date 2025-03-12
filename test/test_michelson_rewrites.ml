
module Rewriter = Michelson_optimisations.Rewriter
module If_suffix_rewriter = Michelson_optimisations.If_suffix_rewriter
open Oasis_core.Michelson

let oasis_micheline_to_micheline oasis_micheline =
  let micheline = Rewriter.oasis_micheline_to_micheline oasis_micheline in
  let micheline = Tezos_micheline.Micheline.map_node (fun x -> x) (fun prim -> Michelson.Ast.Prim.of_string prim) (If_suffix_rewriter.optimize (Tezos_micheline.Micheline.map_node (fun x -> x) (fun prim -> Michelson.Ast.Prim.to_string prim) micheline)) in
  micheline

let make_instructions instr = 
  let micheline =
    Oasis_core.Micheline.Sequence (
      Oasis_core.Michelson.To_micheline.instruction (
        {instr = MIseq (
          List.map ~f:(fun i -> {instr = i}) instr 
        )}
      )
    ) in
  micheline

let test_instructions instructions =
  let oasis_micheline = make_instructions instructions in

  let code = Oasis_core.Michelson.Of_micheline.instruction oasis_micheline in
  let optimised_code = Oasis_core.Michelson_rewriter.run (Oasis_core.Michelson_rewriter.simplify) code in
  let oasis_micheline_list = Oasis_core.Michelson.To_micheline.instruction optimised_code in
  let optimised_oasis_micheline = Oasis_core.Micheline.Sequence (oasis_micheline_list) in

  Test_nodes.print_instructions [oasis_micheline_to_micheline oasis_micheline];
  Printf.printf "->\n";
  Test_nodes.print_instructions [oasis_micheline_to_micheline optimised_oasis_micheline];

  (* Check if second pass differs from first pass. If it does, print it. *)
  let second_optimised_code = Oasis_core.Michelson_rewriter.run (Oasis_core.Michelson_rewriter.simplify) optimised_code in
  let second_optimised_micheline_list = Oasis_core.Michelson.To_micheline.instruction second_optimised_code in 
  let second_optimised_oasis_micheline = Oasis_core.Micheline.Sequence second_optimised_micheline_list in

  if second_optimised_oasis_micheline <> optimised_oasis_micheline then begin
    Printf.printf "Further optimisations were possible:\n";
    Test_nodes.print_instructions [oasis_micheline_to_micheline optimised_oasis_micheline];
    Printf.printf "->\n";
    Test_nodes.print_instructions [oasis_micheline_to_micheline second_optimised_oasis_micheline]
  end

let%expect_test "print_expr" =
  test_instructions [MIdup 1;  MIdig 0; MIdup 1];
  [%expect {|
    { DUP ; DIG 0 ; DUP }
    ->
    { DUP ; DUP } |}]

let test = test_instructions

(* instr_to_push *)

let%expect_test "instr_to_push_unit" =
  test [MI0 Unit_];
  [%expect {|
    { UNIT }
    ->
    { UNIT } |}]

let%expect_test "instr_to_push_none" =
  test [MI0 (None_ mt_nat)];
  [%expect {|
    { NONE nat }
    ->
    { NONE nat } |}]

let%expect_test "instr_to_push_nil" =
  test [MI0 (Nil mt_string)];
  [%expect {|
    { NIL string }
    ->
    { NIL string } |}]

let%expect_test "instr_to_push_empty_set" =
  test [MI0 (Empty_set mt_bytes)];
  [%expect {|
    { EMPTY_SET bytes }
    ->
    { EMPTY_SET bytes } |}]

let%expect_test "instr_to_push_empty_map" =
  test [MI0 (Empty_map (mt_string, mt_int))];
  [%expect {|
    { EMPTY_MAP string int }
    ->
    { EMPTY_MAP string int } |}]

(* push_to_instr *)

let%expect_test "push_to_instr_unit" =
  test [MIpush (mt_unit, MLiteral.unit)];
  [%expect {|
    { PUSH unit Unit }
    ->
    { UNIT } |}]

let%expect_test "push_to_instr_none" =
  test [MIpush (mt_option mt_nat, MLiteral.none)];
  [%expect {|
    { PUSH (option nat) None }
    ->
    { NONE nat } |}]

let%expect_test "push_to_instr_nil" =
  test [MIpush (mt_list mt_string, MLiteral.list [])];
  [%expect {|
    { PUSH (list string) {} }
    ->
    { NIL string } |}]

let%expect_test "push_to_instr_empty_set" =
  test [MIpush (mt_set mt_bytes, MLiteral.set [])];
  [%expect {|
    { PUSH (set bytes) {} }
    ->
    { EMPTY_SET bytes } |}]

let%expect_test "push_to_instr_empty_map" =
  test [MIpush (mt_map mt_string mt_int, MLiteral.mk_map [])];
  [%expect {|
    { PUSH (map string int) {} }
    ->
    { PUSH (map string int) {} } |}]

(* unfold_macros *)

let%expect_test "unfold_macros_setfield_a" =
  test [MIsetField [A]];
  [%expect {|
    { CDR ; SWAP ; PAIR }
    ->
    { CDR ; SWAP ; PAIR } |}]

let%expect_test "unfold_macros_setfield_d" =
  test [MIsetField [D]];
  [%expect {|
    { CAR ; PAIR }
    ->
    { CAR ; PAIR } |}]

let%expect_test "unfold_macros_setfield_complex" =
  test [MIsetField [A; D]];
  [%expect {|
    { DUP ; DIP { CAR ; CAR ; PAIR } ; CDR ; SWAP ; PAIR }
    ->
    { DUP ; DIP { CAR ; CAR ; PAIR } ; CDR ; SWAP ; PAIR } |}]

let%expect_test "unfold_macros_pairn_2" =
  test [MIpairn 2];
  [%expect {|
    { PAIR 2 }
    ->
    { PAIR } |}]

(* unfold_mifield *)

let%expect_test "unfold_mifield_simple" =
  test [MIfield [D; A]];
  [%expect {|
    { CDR ; CAR }
    ->
    { GET 3 } |}]

let%expect_test "unfold_mifield_getn_1" =
  test [MI1 (Getn 1)];
  [%expect {|
    { GET 1 }
    ->
    { CAR } |}]

let%expect_test "unfold_mifield_getn_2" =
  test [MI1 (Getn 2)];
  [%expect {|
    { GET 2 }
    ->
    { CDR } |}]

(* unfold_selective_unpair *)

let%expect_test "unfold_selective_unpair_true_false" =
  test [MIunpair [true; false]];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure int_of_string)
  Raised by primitive operation at Oasis_core__Michelson.Of_micheline.instruction in file "lib/michelson/optimisations/oasis_core/michelson.ml", line 2078, characters 31-48
  Called from Oasis_core__Michelson.Of_micheline.instruction in file "lib/michelson/optimisations/oasis_core/michelson.ml", line 2017, characters 26-41
  Called from Test_dsl__Test_michelson_rewrites.test_instructions in file "test/test_michelson_rewrites.ml", line 25, characters 13-74
  Called from Test_dsl__Test_michelson_rewrites.(fun) in file "test/test_michelson_rewrites.ml", line 134, characters 2-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]

let%expect_test "unfold_selective_unpair_many" =
  test [MIunpair [true; true; false]];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure int_of_string)
  Raised by primitive operation at Oasis_core__Michelson.Of_micheline.instruction in file "lib/michelson/optimisations/oasis_core/michelson.ml", line 2078, characters 31-48
  Called from Oasis_core__Michelson.Of_micheline.instruction in file "lib/michelson/optimisations/oasis_core/michelson.ml", line 2017, characters 26-41
  Called from Test_dsl__Test_michelson_rewrites.test_instructions in file "test/test_michelson_rewrites.ml", line 25, characters 13-74
  Called from Test_dsl__Test_michelson_rewrites.(fun) in file "test/test_michelson_rewrites.ml", line 138, characters 2-37
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]