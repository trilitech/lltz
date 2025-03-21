
module Rewriter = Michelson_optimisations.Rewriter
module If_suffix_rewriter = Michelson_optimisations.If_suffix_rewriter
open Smartpy_core.Michelson

let oasis_micheline_to_micheline oasis_micheline =
  let micheline = Rewriter.oasis_micheline_to_micheline oasis_micheline in
  let micheline = Tezos_micheline.Micheline.map_node (fun x -> x) (fun prim -> Lltz_michelson.Ast.Prim.of_string prim) (If_suffix_rewriter.optimize (Tezos_micheline.Micheline.map_node (fun x -> x) (fun prim -> Lltz_michelson.Ast.Prim.to_string prim) micheline)) in
  micheline

let make_instructions instr = 
  let micheline =
    Smartpy_core.Micheline.Sequence (
      Smartpy_core.Michelson.To_micheline.instruction (
        {instr = MIseq (
          List.map ~f:(fun i -> {instr = i}) instr 
        )}
      )
    ) in
  micheline

let test_instructions instructions =
  let oasis_micheline = make_instructions instructions in

  let code = Smartpy_core.Michelson.Of_micheline.instruction oasis_micheline in
  let optimised_code = Smartpy_core.Michelson_rewriter.run (Smartpy_core.Michelson_rewriter.simplify) code in
  let oasis_micheline_list = Smartpy_core.Michelson.To_micheline.instruction optimised_code in
  let optimised_oasis_micheline = Smartpy_core.Micheline.Sequence (oasis_micheline_list) in

  Test_nodes.print_instructions [oasis_micheline_to_micheline oasis_micheline];
  Printf.printf "->\n";
  Test_nodes.print_instructions [oasis_micheline_to_micheline optimised_oasis_micheline];

  (* Check if second pass differs from first pass. If it does, print it. *)
  let second_optimised_code = Smartpy_core.Michelson_rewriter.run (Smartpy_core.Michelson_rewriter.simplify) optimised_code in
  let second_optimised_micheline_list = Smartpy_core.Michelson.To_micheline.instruction second_optimised_code in 
  let second_optimised_oasis_micheline = Smartpy_core.Micheline.Sequence second_optimised_micheline_list in

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
  test [MIunpair [true; true]];
  [%expect{|
    { UNPAIR }
    ->
    { UNPAIR } |}]

let%expect_test "unfold_selective_unpair_many" =
test [MIunpair [true; true;]; MIdropn 1];
[%expect{|
  { UNPAIR ; DROP 1 }
  ->
  { CDR } |}]

let%expect_test "unfold_selective_unpair_many" =
  test [MIunpair [true; true; true]; MIdropn 1];
  [%expect{|
    { UNPAIR 3 ; DROP 1 }
    ->
    { CDR ; UNPAIR } |}]

let%expect_test "fold_getn_d_a" =
  test [MIfield [D]; MIfield [A]];
  [%expect {|
    { CDR ; CAR }
    ->
    { GET 3 } |}]

let%expect_test "fold_getn_d_d" =
  test [MIfield [D]; MIfield [D]];
  [%expect {|
    { CDR ; CDR }
    ->
    { GET 4 } |}]

let%expect_test "fold_getn_d_then_getn" =
  test [MIfield [D]; MI1 (Getn 5)];
  [%expect {|
    { CDR ; GET 5 }
    ->
    { GET 7 } |}]

let%expect_test "fold_getn_getn_even" =
  test [MI1 (Getn 4); MI1 (Getn 2)];
  [%expect {|
    { GET 4 ; GET 2 }
    ->
    { GET 4 ; CDR } |}]

(* fold_dropn *)
let%expect_test "fold_dropn_simple" =
  test [MIdrop; MIdrop];
  [%expect {|
    { DROP ; DROP }
    ->
    { DROP 2 } |}]

let%expect_test "fold_dropn_drop_dropn" =
  test [MIdrop; MIdropn 3];
  [%expect {|
    { DROP ; DROP 3 }
    ->
    { DROP 4 } |}]

let%expect_test "fold_dropn_dropn_drop" =
  test [MIdropn 2; MIdrop];
  [%expect {|
    { DROP 2 ; DROP }
    ->
    { DROP 3 } |}]

let%expect_test "fold_dropn_dropn_dropn" =
  test [MIdropn 2; MIdropn 3];
  [%expect {|
    { DROP 2 ; DROP 3 }
    ->
    { DROP 5 } |}]

(* unfold_dropn *)
let%expect_test "unfold_dropn_1" =
  test [MIdropn 1];
  [%expect {|
    { DROP 1 }
    ->
    { DROP } |}]

let%expect_test "unfold_dropn_many" =
  test [MIdropn 3];
  [%expect {|
    { DROP 3 }
    ->
    { DROP 3 } |}]

(* push_push *)
let%expect_test "push_push_same_literal" =
  test
    [ MIpush (mt_nat, MLiteral.small_int 42)
    ; MIpush (mt_nat, MLiteral.small_int 42)
    ];
  [%expect {|
    { PUSH nat 42 ; PUSH nat 42 }
    ->
    { PUSH nat 42 ; DUP } |}]

let%expect_test "push_push_different_literal" =
  test
    [ MIpush (mt_string, MLiteral.string "foo")
    ; MIpush (mt_string, MLiteral.string "bar")
    ];
  [%expect {|
    { PUSH string "foo" ; PUSH string "bar" }
    ->
    { PUSH string "foo" ; PUSH string "bar" } |}]

(* push_zero_compare *)
let%expect_test "push_zero_compare_nat_eq" =
  test
    [ MIpush (mt_nat, MLiteral.small_int 0)
    ; MIdig 1
    ; MI2 Compare
    ; MI1 Eq
    ];
  [%expect {|
    { PUSH nat 0 ; DIG 1 ; COMPARE ; EQ }
    ->
    { PUSH nat 0 ; COMPARE ; EQ } |}]

let%expect_test "push_zero_compare_int_ge" =
  test
    [ MIpush (mt_int, MLiteral.small_int 0)
    ; MIdup 2
    ; MI2 Compare
    ; MI1 Ge
    ];
  [%expect {|
    { PUSH int 0 ; DUP 2 ; COMPARE ; GE }
    ->
    { DUP ; GE } |}]

(* dig1_to_swap *)
let%expect_test "dig1_to_swap" =
  test [MIdig 1];
  [%expect {|
    { DIG 1 }
    ->
    { SWAP } |}]

(* swap_to_dig1 *)
let%expect_test "swap_to_dig1" =
  test [MIswap];
  [%expect {|
    { SWAP }
    ->
    { SWAP } |}]

let mk_instr instr = { instr }

let%expect_test "cond_check_last_same_push_if" =
  test [
    MIif (
      { instr = MIseq [
          mk_instr (MIdig 2); 
          mk_instr (MIpush (mt_int, MLiteral.small_int 42));
        ]},
      { instr = MIseq [
          mk_instr (MIdig 1); 
          mk_instr (MIpush (mt_int, MLiteral.small_int 42));
        ]}
    )
  ];
  [%expect {|
    { IF { DIG 2 ; PUSH int 42 } { DIG 1 ; PUSH int 42 } }
    ->
    { IF { DIG 2 } { SWAP } ; PUSH int 42 } |}]

let%expect_test "cond_check_last_push_fail_if" =
  test [
    MIif (
      { instr = MIseq [
          mk_instr (MIdup 1);
          mk_instr (MIpush (mt_string, MLiteral.string "failed"));
        ]},
      { instr = MIseq [
          mk_instr (MI1_fail Failwith)
        ]}
    )
  ];
  [%expect {|
    { IF { DUP ; PUSH string "failed" } { FAILWITH } }
    ->
    { IF { DUP } { FAILWITH } ; PUSH string "failed" } |}]

let%expect_test "cond_check_last_fail_push_if" =
  test [
    MIif (
      { instr = MIseq [
          mk_instr (MI1_fail Failwith)
        ]},
      { instr = MIseq [
          mk_instr (MIdig 2);
          mk_instr (MIpush (mt_nat, MLiteral.small_int 123));
        ]}
    )
  ];
  [%expect {|
    { IF { FAILWITH } { DIG 2 ; PUSH nat 123 } }
    ->
    { IF { FAILWITH } { DIG 2 } ; PUSH nat 123 } |}]

let%expect_test "cond_check_last_same_push_if_none" =
  test [
    MIif_none (
      { instr = MIseq [
          mk_instr (MIdig 3); 
          mk_instr (MIpush (mt_int, MLiteral.small_int 99));
        ]},
      { instr = MIseq [
          mk_instr (MIdig 1); 
          mk_instr (MIpush (mt_int, MLiteral.small_int 99));
        ]}
    )
  ];
  [%expect {|
    { IF_NONE { DIG 3 ; PUSH int 99 } { DIG 1 ; PUSH int 99 } }
    ->
    { IF_NONE { DIG 3 } { SWAP } ; PUSH int 99 } |}]

let%expect_test "cond_check_last_push_fail_if_none" =
  test [
    MIif_none (
      { instr = MIseq [
          mk_instr (MIdrop);
          mk_instr (MIpush (mt_string, MLiteral.string "if-none case"));
        ]},
      { instr = MIseq [
          mk_instr (MI1_fail Failwith)
        ]}
    )
  ];
  [%expect {|
    { IF_NONE { DROP ; PUSH string "if-none case" } { FAILWITH } }
    ->
    { IF_NONE { DROP } { FAILWITH } ; PUSH string "if-none case" } |}]

let%expect_test "cond_check_last_fail_push_if_none" =
  test [
    MIif_none (
      { instr = MIseq [
          mk_instr (MI1_fail Failwith)
        ]},
      { instr = MIseq [
          mk_instr (MIdup 1);
          mk_instr (MIpush (mt_unit, MLiteral.unit));
        ]}
    )
  ];
  [%expect {|
    { IF_NONE { FAILWITH } { DUP ; PUSH unit Unit } }
    ->
    { IF_NONE { FAILWITH } { DUP } ; UNIT } |}]

let%expect_test "cond_check_last_same_push_if_left" =
  test [
    MIif_left (
      { instr = MIseq [
          mk_instr (MIdrop);
          mk_instr (MIpush (mt_bool, MLiteral.bool true));
        ]},
      { instr = MIseq [
          mk_instr (MIdup 1);
          mk_instr (MIpush (mt_bool, MLiteral.bool true));
        ]}
    )
  ];
  [%expect {|
    { IF_LEFT { DROP ; PUSH bool True } { DUP ; PUSH bool True } }
    ->
    { IF_LEFT { DROP } { DUP } ; PUSH bool True } |}]

let%expect_test "cond_check_last_push_fail_if_left" =
  test [
    MIif_left (
      { instr = MIseq [
          mk_instr (MIdig 2);
          mk_instr (MIpush (mt_string, MLiteral.string "left"));
        ]},
      { instr = MIseq [
          mk_instr (MI1_fail Failwith)
        ]}
    )
  ];
  [%expect {|
    { IF_LEFT { DIG 2 ; PUSH string "left" } { FAILWITH } }
    ->
    { IF_LEFT { DIG 2 } { FAILWITH } ; PUSH string "left" } |}]

let%expect_test "cond_check_last_fail_push_if_left" =
  test [
    MIif_left (
      { instr = MIseq [
          mk_instr (MI1_fail Failwith)
        ]},
      { instr = MIseq [
          mk_instr (MIdig 3);
          mk_instr (MIpush (mt_nat, MLiteral.small_int 7));
        ]}
    )
  ];
  [%expect {|
    { IF_LEFT { FAILWITH } { DIG 3 ; PUSH nat 7 } }
    ->
    { IF_LEFT { FAILWITH } { DIG 3 } ; PUSH nat 7 } |}]

let%expect_test "cond_check_last_same_push_if_cons" =
  test [
    MIif_cons (
      { instr = MIseq [
          mk_instr (MIdup 2);
          mk_instr (MIpush (mt_string, MLiteral.string "cons-branch"));
        ]},
      { instr = MIseq [
          mk_instr (MIdig 1);
          mk_instr (MIpush (mt_string, MLiteral.string "cons-branch"));
        ]}
    )
  ];
  [%expect {|
    { IF_CONS
        { DUP 2 ; PUSH string "cons-branch" }
        { DIG 1 ; PUSH string "cons-branch" } }
    ->
    { IF_CONS { DUP 2 } { SWAP } ; PUSH string "cons-branch" } |}]

let%expect_test "cond_check_last_push_fail_if_cons" =
  test [
    MIif_cons (
      { instr = MIseq [
          mk_instr (MIdig 4);
          mk_instr (MIpush (mt_int, MLiteral.small_int 2023));
        ]},
      { instr = MIseq [
          mk_instr (MI1_fail Failwith)
        ]}
    )
  ];
  [%expect {|
    { IF_CONS { DIG 4 ; PUSH int 2023 } { FAILWITH } }
    ->
    { IF_CONS { DIG 4 } { FAILWITH } ; PUSH int 2023 } |}]

let%expect_test "cond_check_last_fail_push_if_cons" =
  test [
    MIif_cons (
      { instr = MIseq [
          mk_instr (MI1_fail Failwith)
        ]},
      { instr = MIseq [
          mk_instr (MIdig 2);
          mk_instr (MIpush (mt_unit, MLiteral.unit));
        ]}
    )
  ];
  [%expect {|
    { IF_CONS { FAILWITH } { DIG 2 ; PUSH unit Unit } }
    ->
    { IF_CONS { FAILWITH } { DIG 2 } ; UNIT } |}]

let mk i = { instr = i }

let%expect_test "if_with_left_branch_prefix_drop_and_right_branch_fails" =
  test_instructions
    [ MIif
        ( { instr = MIdrop }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "failing branch"))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF { DROP } { PUSH string "failing branch" ; FAILWITH } }
    ->
    { SWAP ; DROP ; IF {} { PUSH string "failing branch" ; FAILWITH } } |}]

let%expect_test "if_with_right_branch_prefix_drop_and_left_branch_fails" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "left fails"))
                ; mk (MI1_fail Failwith)
                ]
          }
        , { instr = MIdrop } )
    ];
  [%expect {|
    { IF { PUSH string "left fails" ; FAILWITH } { DROP } }
    ->
    { SWAP ; DROP ; IF { PUSH string "left fails" ; FAILWITH } {} } |}]

let%expect_test "if_with_drop_appended_after_the_if" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MIpush (mt_int, MLiteral.small_int 1))
                ; mk (MI2 (Add))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_int, MLiteral.small_int 2))
                ; mk (MI2 (Mul))
                ]
          } )
    ; MIdrop
    ];
  [%expect {|
    { IF { PUSH int 1 ; ADD } { PUSH int 2 ; MUL } ; DROP }
    ->
    { IF { PUSH int 1 ; ADD ; DROP } { PUSH int 2 ; MUL ; DROP } } |}]

let%expect_test "if_none_with_drop_appended" =
  test_instructions
    [ MIif_none
        ( { instr = MIseq [ mk (MIdig 1); mk (MI1 Neg) ] }
        , { instr = MIseq [ mk (MIdig 2); mk (MI2 (Sub)) ] } )
    ; MIdrop
    ];
  [%expect {|
    { IF_NONE { DIG 1 ; NEG } { DIG 2 ; SUB } ; DROP }
    ->
    { IF_NONE { SWAP ; DROP } { DIG 2 ; SUB ; DROP } } |}]

let%expect_test "if_left_with_drop_appended" =
  test_instructions
    [ MIif_left
        ( { instr = MIdrop }
        , { instr =
              MIseq
                [ mk (MIpush (mt_nat, MLiteral.small_int 42))
                ; mk (MI1 Int)
                ]
          } )
    ; MIdrop
    ];
  [%expect {|
    { IF_LEFT { DROP } { PUSH nat 42 ; INT } ; DROP }
    ->
    { IF_LEFT { DROP 2 } {} } |}]

let%expect_test "if_cons_with_drop_appended" =
  test_instructions
    [ MIif_cons
        ( { instr =
              MIseq
                [ mk (MIdig 2)
                ; mk (MIswap)
                ]
          }
        , { instr = MIdrop } )
    ; MIdrop
    ];
  [%expect {|
    { IF_CONS { DIG 2 ; SWAP } { DROP } ; DROP }
    ->
    { IF_CONS { DROP ; SWAP } { DROP 2 } } |}]

let%expect_test "if_with_prefix_drop_on_left_and_failing_right" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MIdrop)
                ; mk (MI2 (Or)) 
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "fail branch"))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF { DROP ; OR } { PUSH string "fail branch" ; FAILWITH } }
    ->
    { SWAP ; DROP ; IF { OR } { PUSH string "fail branch" ; FAILWITH } } |}]

let%expect_test "if_with_prefix_drop_on_right_and_failing_left" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "Left is failing"))
                ; mk (MI1_fail Failwith)
                ]
          }
        , { instr =
              MIseq
                [ mk (MIdrop)
                ; mk (MI1 Not)
                ]
          } )
    ];
  [%expect {|
    { IF { PUSH string "Left is failing" ; FAILWITH } { DROP ; NOT } }
    ->
    { SWAP ;
      DROP ;
      IF { PUSH string "Left is failing" ; FAILWITH } { NOT } } |}]

let%expect_test "if_none_with_prefix_drop_on_left_and_failing_right" =
  test_instructions
    [ MIif_none
        ( { instr =
              MIseq
                [ mk (MIdrop)
                ; mk (MIdup 2)
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "right fails"))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF_NONE { DROP ; DUP 2 } { PUSH string "right fails" ; FAILWITH } }
    ->
    { IF_NONE { DROP ; DUP 2 } { PUSH string "right fails" ; FAILWITH } } |}]

let%expect_test "if_left_with_prefix_drop_on_right_and_failing_left" =
  test_instructions
    [ MIif_left
        ( { instr = MIseq [ mk (MI1_fail Failwith) ] }
        , { instr =
              MIseq
                [ mk (MIdrop)
                ; mk (MIdig 3)
                ]
          } )
    ];
  [%expect {|
    { IF_LEFT { FAILWITH } { DROP ; DIG 3 } }
    ->
    { IF_LEFT { FAILWITH } { DROP ; DIG 3 } } |}]

let%expect_test "if_cons_with_prefix_drop_on_left_and_failing_right" =
  test_instructions
    [ MIif_cons
        ( { instr =
              MIseq
                [ mk (MIdrop)
                ; mk (MIpush (mt_bool, MLiteral.bool true))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "failing"))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF_CONS { DROP ; PUSH bool True } { PUSH string "failing" ; FAILWITH } }
    ->
    { IF_CONS { DROP } { PUSH string "failing" ; FAILWITH } ;
      PUSH bool True } |}]

let%expect_test "remove_prefix_drop_on_if_left" =
  test_instructions
    [ MIif_left
        ( { instr = MIdrop }
        , { instr = MIseq [ mk (MIpush (mt_unit, MLiteral.unit)) ] } )
    ];
  [%expect {|
    { IF_LEFT { DROP } { PUSH unit Unit } }
    ->
    { IF_LEFT { DROP } { UNIT } } |}]
  
let%expect_test "dig_dug_several_same_DIG" =
  test_instructions
    [ MIdig 2
    ; MIdig 2
    ; MIdig 2
    ; MIdig 2
    ];
  [%expect {|
    { DIG 2 ; DIG 2 ; DIG 2 ; DIG 2 }
    ->
    { DIG 2 } |}]

let%expect_test "dig_dug_several_same_DUG" =
  test_instructions
    [ MIdug 3
    ; MIdug 3
    ; MIdug 3
    ];
  [%expect {|
    { DUG 3 ; DUG 3 ; DUG 3 }
    ->
    { DIG 3 } |}]

let%expect_test "dig_dug_interleave" =
  test_instructions
    [ MIdig 2
    ; MIdig 2
    ; MIdug 2
    ; MIdug 2
    ; MIdig 2
    ];
  [%expect {|
    { DIG 2 ; DIG 2 ; DUG 2 ; DUG 2 ; DIG 2 }
    ->
    { DIG 2 } |}]

let%expect_test "dig_dug_with_swap" =
  test_instructions
    [ MIdig 1
    ; MIdig 1
    ; MIswap
    ; MIdug 1
    ; MIdug 1
    ];
  [%expect {|
    { DIG 1 ; DIG 1 ; SWAP ; DUG 1 ; DUG 1 }
    ->
    { SWAP } |}]

let%expect_test "dig_dug_swap_with_break" =
  test_instructions
    [ MIdig 1
    ; MIpush (mt_nat, MLiteral.small_int 42)
    ; MIswap
    ; MIdug 1
    ];
  [%expect {|
    { DIG 1 ; PUSH nat 42 ; SWAP ; DUG 1 }
    ->
    { SWAP ; PUSH nat 42 } |}]

let%expect_test "dig_dug_with_comments" =
  test_instructions
    [ MIdig 2
    ; MIcomment ["some comment"]
    ; MIdig 2
    ; MIswap
    ; MIdug 2
    ; MIcomment ["another comment"]
    ; MIdug 2
    ];
  [%expect {|
    { DIG 2 ; DIG 2 ; SWAP ; DUG 2 ; DUG 2 }
    ->
    { DUG 2 ; SWAP ; DIG 2 } |}]

let%expect_test "dig_dug_single_DIG" =
  test_instructions
    [ MIdig 3
    ; MIpush (mt_string, MLiteral.string "hello")
    ];
  [%expect {|
    { DIG 3 ; PUSH string "hello" }
    ->
    { DIG 3 ; PUSH string "hello" } |}]

let%expect_test "dig_dug_single_DUG" =
  test_instructions
    [ MIdug 2
    ; MI1 Not
    ; MIdrop
    ];
  [%expect {|
    { DUG 2 ; NOT ; DROP }
    ->
    { SWAP ; DROP ; SWAP } |}]

let%expect_test "dig_dug_cycle_dig" =
  test_instructions
    [ MIdig 3
    ; MIdig 3
    ; MIdig 3
    ];
  [%expect {|
    { DIG 3 ; DIG 3 ; DIG 3 }
    ->
    { DUG 3 } |}]

let%expect_test "dig_dug_cycle_dug" =
  test_instructions
    [ MIdug 2
    ; MIdug 2
    ];
  [%expect {|
    { DUG 2 ; DUG 2 }
    ->
    { DIG 2 } |}]

let%expect_test "if_both_branches_start_with_DIG_DROP_same_n" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "left branch"))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "right branch"))
                ]
          } )
    ];
  [%expect {|
    { IF { DIG 2 ; DROP ; PUSH string "left branch" }
         { DIG 2 ; DROP ; PUSH string "right branch" } }
    ->
    { DIG 3 ;
      DROP ;
      IF { PUSH string "left branch" } { PUSH string "right branch" } } |}]

let%expect_test "if_DIG2_DROP_vs_DIG1_DROP_minmax_like_case" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_int, MLiteral.small_int 100))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIdig 1)
                ; mk MIdrop
                ; mk (MIdig 1)
                ; mk MIdrop
                ; mk (MIpush (mt_int, MLiteral.small_int 200))
                ]
          } )
    ];
  [%expect {|
    { IF { DIG 2 ; DROP ; PUSH int 100 }
         { DIG 1 ; DROP ; DIG 1 ; DROP ; PUSH int 200 } }
    ->
    { DIG 3 ; DROP ; IF { PUSH int 100 } { SWAP ; DROP ; PUSH int 200 } } |}]

let%expect_test "if_min_max_pattern" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIdig 3)
                ; mk MIdrop
                ]
          }
        , { instr =
              MIseq
                [ mk (MIdig 1)
                ; mk MIdrop
                ; mk (MIdig 3)
                ; mk MIdrop
                ]
          } )
    ];
  [%expect {|
    { IF { DROP ; DIG 3 ; DROP } { DIG 1 ; DROP ; DIG 3 ; DROP } }
    ->
    { DIG 5 ; DROP ; IF { DROP } { SWAP ; DROP } } |}]

let%expect_test "if_with_empty_branches" =
  test_instructions
    [ MIif ({ instr = MIseq [] }, { instr = MIseq [] }) ];
  [%expect {|
    { IF {} {} }
    ->
    { DROP } |}]

let%expect_test "if_left_drop_drop_to_drop" =
  test_instructions [ MIif_left (mk MIdrop, mk MIdrop) ];
  [%expect {|
    { IF_LEFT { DROP } { DROP } }
    ->
    { DROP } |}]

let%expect_test "if_cons_double_drop_left_empty_right" =
  test_instructions
    [ MIif_cons
        ( { instr = MIseq [ mk MIdrop; mk MIdrop ] }
        , { instr = MIseq [] } )
    ];
  [%expect {|
    { IF_CONS { DROP ; DROP } {} }
    ->
    { DROP } |}]

let%expect_test "if_left_with_drop_dig_drop_on_both_sides" =
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "L"))
                ]
          }
        , { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "R"))
                ]
          } )
    ];
  [%expect {|
    { IF_LEFT
        { DROP ; DIG 2 ; DROP ; PUSH string "L" }
        { DROP ; DIG 2 ; DROP ; PUSH string "R" } }
    ->
    { DIG 3 ;
      DROP ;
      IF_LEFT { DROP ; PUSH string "L" } { DROP ; PUSH string "R" } } |}]

let%expect_test "not_then_if_swaps_branches" =
  test_instructions
    [ MI1 Not
    ; MIif
        ( { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 1)) ] }
        , { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 2)) ] } )
    ];
  [%expect {|
    { NOT ; IF { PUSH int 1 } { PUSH int 2 } }
    ->
    { IF { PUSH int 2 } { PUSH int 1 } } |}]

let%expect_test "if_push_true_push_false_removed" =
  test_instructions
    [ MIif
        ( mk (MIpush (mt_bool, MLiteral.bool true)),
          mk (MIpush (mt_bool, MLiteral.bool false)) )
    ];
  [%expect {|
    { IF { PUSH bool True } { PUSH bool False } }
    ->
    {} |}]

let%expect_test "if_push_false_push_true_to_not" =
  test_instructions
    [ MIif
        ( mk (MIpush (mt_bool, MLiteral.bool false)),
          mk (MIpush (mt_bool, MLiteral.bool true)) )
    ];
  [%expect {|
    { IF { PUSH bool False } { PUSH bool True } }
    ->
    { NOT } |}]

let%expect_test "if_push_bool_x_then_afterwards_not_on_stack" =
  test_instructions
    [ MIif
        ( mk (MIpush (mt_bool, MLiteral.bool true)),
          mk (MIdrop) )
    ; MI1 Not
    ];
  [%expect {|
    { IF { PUSH bool True } { DROP } ; NOT }
    ->
    { IF { PUSH bool False } { DROP ; NOT } } |}]

let%expect_test "if_drop_appended_afterwards" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "left code"))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "right code"))
                ]
          } )
    ; MIdrop
    ];
  [%expect {|
    { IF { PUSH string "left code" } { PUSH string "right code" } ; DROP }
    ->
    { DROP } |}]

let%expect_test "if_dig_drop_appended_afterwards" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MIpush (mt_nat, MLiteral.small_int 123)) ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "string-literal")) ]
          } )
    ; MIdig 2
    ; MIdrop
    ];
  [%expect {|
    { IF { PUSH nat 123 } { PUSH string "string-literal" } ; DIG 2 ; DROP }
    ->
    { DIG 2 ; DROP ; IF { PUSH nat 123 } { PUSH string "string-literal" } } |}]

let%expect_test "if_left_left_branch_fail_right_branch_dig_drop" =
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk (MI1_fail Failwith) ]
          }
        , { instr =
              MIseq
                [ mk (MIdig 3)
                ; mk MIdrop
                ; mk (MIpush (mt_int, MLiteral.small_int 111))
                ]
          } )
    ];
  [%expect {|
    { IF_LEFT { FAILWITH } { DIG 3 ; DROP ; PUSH int 111 } }
    ->
    { IF_LEFT { FAILWITH } { DIG 3 ; DROP } ; PUSH int 111 } |}]

let%expect_test "if_none_right_branch_fail_left_branch_dig_drop" =
  test_instructions
    [ MIif_none
        ( { instr =
              MIseq
                [ mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "some"))
                ]
          }
        , { instr = MIseq [ mk (MI1_fail Failwith) ] } )
    ];
  [%expect {|
    { IF_NONE { DIG 2 ; DROP ; PUSH string "some" } { FAILWITH } }
    ->
    { IF_NONE { DIG 2 ; DROP } { FAILWITH } ; PUSH string "some" } |}]

let%expect_test "if_none_pair_fail_test" =
  test_instructions
    [ MIif_none
        ( { instr =
              MIseq
                [ mk (MI2 (Pair(None, None)))
                ; mk (MI1_fail Failwith)
                ]
          }
        , { instr =
              MIseq
                [ mk (MIdig 3)
                ; mk MIdrop
                ; mk (MIpush (mt_nat, MLiteral.small_int 999))
                ]
          } )
    ];
  [%expect {|
    { IF_NONE { PAIR ; FAILWITH } { DIG 3 ; DROP ; PUSH nat 999 } }
    ->
    { DIG 3 ; DROP ; IF_NONE { PAIR ; FAILWITH } {} ; PUSH nat 999 } |}]

let%expect_test "if_drop_fail" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk (MI1 Neg)
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "fail msg"))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF { DROP ; NEG } { PUSH string "fail msg" ; FAILWITH } }
    ->
    { SWAP ; DROP ; IF { NEG } { PUSH string "fail msg" ; FAILWITH } } |}]

let%expect_test "if_fail_drop" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk (MI1_fail Failwith)
                ]
          }
        , { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIdup 1)
                ]
          } )
    ];
  [%expect {|
    { IF { FAILWITH } { DROP ; DUP } }
    ->
    { IF { FAILWITH } { DROP ; DUP } } |}]

let%expect_test "if_both_branches_drop_merge_to_single_drop" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk MIdrop
                ]
          }
        , { instr =
              MIseq
                [ mk MIdrop
                ]
          } )
    ];
  [%expect {|
    { IF { DROP ; DROP } { DROP } }
    ->
    { SWAP ; DROP ; IF { DROP } {} } |}]

let%expect_test "if_left_push_int_n_fail_merge" =
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk (MIpush (mt_int, MLiteral.small_int 999))
                ; mk (MI1_fail Failwith)
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_int, MLiteral.small_int 999))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF_LEFT { PUSH int 999 ; FAILWITH } { PUSH int 999 ; FAILWITH } }
    ->
    { PUSH int 999 ; FAILWITH } |}]

let%expect_test "if_left_dig_drop_fail_right" =
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "some left"))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "boom"))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF_LEFT
        { DIG 2 ; DROP ; PUSH string "some left" }
        { PUSH string "boom" ; FAILWITH } }
    ->
    { DIG 2 ;
      DROP ;
      IF_LEFT {} { PUSH string "boom" ; FAILWITH } ;
      PUSH string "some left" } |}]

let%expect_test "if_cons_left_is_dig_drop_fail_right" =
  test_instructions
    [ MIif_cons
        ( { instr =
              MIseq
                [ mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_bool, MLiteral.bool false))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "failbranch"))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF_CONS
        { DIG 2 ; DROP ; PUSH bool False }
        { PUSH string "failbranch" ; FAILWITH } }
    ->
    { SWAP ;
      DROP ;
      IF_CONS {} { PUSH string "failbranch" ; FAILWITH } ;
      PUSH bool False } |}]

let%expect_test "if_left_dig1_drop_many" =
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk (MIdig 1)
                ; mk MIdrop
                ; mk (MIpush (mt_unit, MLiteral.unit))
                ]
          }
        , { instr =
              MIseq
                [ mk MIdrop
                ; mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "hello"))
                ]
          } )
    ];
  [%expect {|
    { IF_LEFT
        { DIG 1 ; DROP ; PUSH unit Unit }
        { DROP ; DROP ; PUSH string "hello" } }
    ->
    { SWAP ; DROP ; IF_LEFT { UNIT } { DROP ; PUSH string "hello" } } |}]

let%expect_test "if_multiple_drop_on_left_and_right" =
  test_instructions
    [ MIif_none
        ( { instr =
              MIseq [ mk MIdrop; mk (MIdup 1) ]
          }
        , { instr =
              MIseq [ mk (MIdig 1); mk MIdrop; mk (MIpush (mt_nat, MLiteral.small_int 42)) ]
          } )
    ];
  [%expect {|
    { IF_NONE { DROP ; DUP } { DIG 1 ; DROP ; PUSH nat 42 } }
    ->
    { SWAP ; DROP ; IF_NONE { DUP } { PUSH nat 42 } } |}]

let%expect_test "if_left_drop_drop_and_drop_drop" =
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk MIdrop
                ; mk (MIpush (mt_int, MLiteral.small_int 10))
                ]
          }
        , { instr =
              MIseq
                [ mk MIdrop
                ; mk MIdrop
                ; mk (MIpush (mt_int, MLiteral.small_int 20))
                ]
          } )
    ];
  [%expect {|
    { IF_LEFT { DROP ; DROP ; PUSH int 10 } { DROP ; DROP ; PUSH int 20 } }
    ->
    { SWAP ;
      DROP ;
      IF_LEFT { DROP ; PUSH int 10 } { DROP ; PUSH int 20 } } |}]

let%expect_test "if_none_bool_flip" =
  test_instructions
    [ MIif_none
        ( { instr = (MIpush (mt_bool, MLiteral.bool false)) }
        , { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIpush (mt_bool, MLiteral.bool true))
                ]
          } )
    ; MIif
        ( { instr = MIseq [ mk (MIpush (mt_string, MLiteral.string "x")) ] },
          { instr = MIseq [ mk (MIpush (mt_string, MLiteral.string "y")) ] } )
    ];
  [%expect {|
    { IF_NONE { PUSH bool False } { DROP ; PUSH bool True } ;
      IF { PUSH string "x" } { PUSH string "y" } }
    ->
    { IF_NONE { PUSH string "y" } { DROP ; PUSH string "x" } } |}]

let%expect_test "nested_if_in_one_branch" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk
                    (MIif
                       ( mk (MIpush (mt_int, MLiteral.small_int 1)),
                         mk (MIpush (mt_int, MLiteral.small_int 2)) ))
                ]
          }
        , { instr = MIdrop } )
    ];
  [%expect {|
    { IF { IF { PUSH int 1 } { PUSH int 2 } } { DROP } }
    ->
    { IF { IF { PUSH int 1 } { PUSH int 2 } } { DROP } } |}]

let%expect_test "multiple_if_in_a_row" =
  test_instructions
    [ MIif
        ( { instr = (MIpush (mt_string, MLiteral.string "A")) },
          { instr = (MIpush (mt_string, MLiteral.string "B")) } )
    ; MIif
        ( { instr = (MIpush (mt_string, MLiteral.string "C")) },
          { instr = (MIpush (mt_string, MLiteral.string "D")) } )
    ];
  [%expect {|
    { IF { PUSH string "A" } { PUSH string "B" } ;
      IF { PUSH string "C" } { PUSH string "D" } }
    ->
    { IF { PUSH string "A" } { PUSH string "B" } ;
      IF { PUSH string "C" } { PUSH string "D" } } |}]

let%expect_test "if_none_nested_if_left_branch" =
  test_instructions
    [ MIif_none
        ( { instr =
              MIseq
                [ mk
                    (MIif
                       ( mk (MIpush (mt_bool, MLiteral.bool true)),
                         mk (MIpush (mt_bool, MLiteral.bool false)) ))
                ]
          }
        , mk (MIpush (mt_nat, MLiteral.small_int 42)) )
    ];
  [%expect {|
    { IF_NONE
        { IF { PUSH bool True } { PUSH bool False } }
        { PUSH nat 42 } }
    ->
    { IF_NONE {} { PUSH nat 42 } } |}]

let%expect_test "if_left_with_lambda_in_branch" =
  let lam_body = { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 1)) ] } in
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk (MIlambda (mt_nat, mt_int, lam_body))
                ; mk (MI2 Exec)
                ]
          }
        , mk (MIpush (mt_string, MLiteral.string "right")) )
    ];
  [%expect {|
    { IF_LEFT { LAMBDA nat int { PUSH int 1 } ; EXEC } { PUSH string "right" } }
    ->
    { IF_LEFT { LAMBDA nat int { PUSH int 1 } ; EXEC } { PUSH string "right" } } |}]

let%expect_test "if_left_with_lam_and_fail" =
  let lam_body = { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 123)) ] } in
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk (MIlambda (mt_nat, mt_int, lam_body))
                ; mk (MI1_fail Failwith)
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_int, MLiteral.small_int 999))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF_LEFT
        { LAMBDA nat int { PUSH int 123 } ; FAILWITH }
        { PUSH int 999 ; FAILWITH } }
    ->
    { IF_LEFT
        { LAMBDA nat int { PUSH int 123 } ; FAILWITH }
        { PUSH int 999 ; FAILWITH } } |}]

let%expect_test "if_cons_with_nested_condition_in_left" =
  test_instructions
    [ MIif_cons
        ( { instr =
              MIseq
                [ mk
                    (MIif
                       ( mk (MIpush (mt_bool, MLiteral.bool false)),
                         mk (MIpush (mt_bool, MLiteral.bool true)) ))
                ]
          }
        , mk MIdrop )
    ];
  [%expect {|
    { IF_CONS { IF { PUSH bool False } { PUSH bool True } } { DROP } }
    ->
    { IF_CONS { NOT } { DROP } } |}]

let%expect_test "multiple_not_in_a_row_then_if" =
  test_instructions
    [ MI1 Not
    ; MI1 Not
    ; MIif
        ( mk (MIpush (mt_bool, MLiteral.bool false)),
          mk (MIpush (mt_bool, MLiteral.bool true)) )
    ];
  [%expect {|
    { NOT ; NOT ; IF { PUSH bool False } { PUSH bool True } }
    ->
    { NOT ; NOT ; NOT } |}]

let%expect_test "if_min_max_pattern_double" =
  test_instructions
    [ MI1 Neg
    ; MIif
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_nat, MLiteral.small_int 10))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIdig 1)
                ; mk MIdrop
                ; mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_nat, MLiteral.small_int 20))
                ]
          } )
    ; MI1 Neg
    ];
  [%expect {|
    { NEG ;
      IF { DROP ; DIG 2 ; DROP ; PUSH nat 10 }
         { DIG 1 ; DROP ; DIG 2 ; DROP ; PUSH nat 20 } ;
      NEG }
    ->
    { NEG ;
      IF { DROP ; DIG 2 ; DROP ; PUSH nat 10 }
         { SWAP ; DROP ; DIG 2 ; DROP ; PUSH nat 20 } ;
      NEG } |}]

let%expect_test "if_drop_on_one_branch_and_nested_if_on_other" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "some leftover"))
                ]
          }
        , { instr =
              MIseq
                [ mk
                    (MIif
                       ( mk (MIpush (mt_nat, MLiteral.small_int 123)),
                         mk (MIpush (mt_nat, MLiteral.small_int 456)) ))
                ]
          } )
    ];
  [%expect {|
    { IF { DROP ; PUSH string "some leftover" }
         { IF { PUSH nat 123 } { PUSH nat 456 } } }
    ->
    { IF { DROP ; PUSH string "some leftover" }
         { IF { PUSH nat 123 } { PUSH nat 456 } } } |}]

let%expect_test "if_with_chained_drops_and_fail" =
  test_instructions
    [ MIif
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk MIdrop
                ; mk (MIpush (mt_int, MLiteral.small_int 111))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_string, MLiteral.string "fail msg"))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF { DROP ; DROP ; PUSH int 111 } { PUSH string "fail msg" ; FAILWITH } }
    ->
    { DUG 2 ;
      DROP 2 ;
      IF {} { PUSH string "fail msg" ; FAILWITH } ;
      PUSH int 111 } |}]

let%expect_test "if_none_push_true_push_false_not_appended" =
  test_instructions
    [ MIif_none
        ( mk (MIpush (mt_bool, MLiteral.bool true)),
          mk (MIpush (mt_bool, MLiteral.bool false)) )
    ; MI1 Not
    ];
  [%expect {|
    { IF_NONE { PUSH bool True } { PUSH bool False } ; NOT }
    ->
    { IF_NONE { PUSH bool True } { PUSH bool False } ; NOT } |}]

let%expect_test "if_none_multiple_branches_with_drop_extra" =
  test_instructions
    [ MIif_none
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "some data"))
                ]
          }
        , { instr =
              MIseq
                [ mk (MIdig 2)
                ; mk MIdrop
                ; mk (MIpush (mt_string, MLiteral.string "other data"))
                ]
          } )
    ];
  [%expect {|
    { IF_NONE
        { DROP ; PUSH string "some data" }
        { DIG 2 ; DROP ; PUSH string "other data" } }
    ->
    { IF_NONE
        { DROP ; PUSH string "some data" }
        { DIG 2 ; DROP ; PUSH string "other data" } } |}]

let%expect_test "if_cons_drop_drop_vs_nested_if" =
  test_instructions
    [ MIif_cons
        ( { instr = MIseq [ mk MIdrop; mk MIdrop ] },
          { instr =
              MIseq
                [ mk
                    (MIif
                       ( mk (MIpush (mt_int, MLiteral.small_int 1000)),
                         mk (MIpush (mt_int, MLiteral.small_int 2000)) ))
                ] } )
    ];
  [%expect {|
    { IF_CONS { DROP ; DROP } { IF { PUSH int 1000 } { PUSH int 2000 } } }
    ->
    { IF_CONS { DROP 2 } { IF { PUSH int 1000 } { PUSH int 2000 } } } |}]

let%expect_test "if_cons_push_bool_false_fail_merge" =
  test_instructions
    [ MIif_cons
        ( { instr =
              MIseq
                [ mk (MIpush (mt_bool, MLiteral.bool false))
                ; mk (MI1_fail Failwith)
                ]
          }
        , { instr =
              MIseq
                [ mk (MIpush (mt_bool, MLiteral.bool false))
                ; mk (MI1_fail Failwith)
                ]
          } )
    ];
  [%expect {|
    { IF_CONS { PUSH bool False ; FAILWITH } { PUSH bool False ; FAILWITH } }
    ->
    { IF_CONS { PUSH bool False ; FAILWITH } { PUSH bool False ; FAILWITH } } |}]

let%expect_test "if_left_drop_dig1_drop_left_vs_simple_right" =
  test_instructions
    [ MIif_left
        ( { instr =
              MIseq
                [ mk MIdrop
                ; mk (MIdig 1)
                ; mk MIdrop
                ]
          }
        , mk (MIpush (mt_nat, MLiteral.small_int 42)) )
    ];
  [%expect {|
    { IF_LEFT { DROP ; DIG 1 ; DROP } { PUSH nat 42 } }
    ->
    { IF_LEFT { DROP ; SWAP ; DROP } { PUSH nat 42 } } |}]

let%expect_test "remove_comments_single" =
  test_instructions
    [ MIcomment ["this is a comment"] ];
  [%expect {|
    {}
    ->
    {} |}]

let%expect_test "remove_comments_interleaved" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 42)
    ; MIcomment ["comment1"]
    ; MIdrop
    ; MIcomment ["comment2"; "comment3"]
    ; MIdup 1
    ];
  [%expect {|
    { PUSH int 42 ; DROP ; DUP }
    ->
    { DUP } |}]

let%expect_test "remove_comments_sequences" =
  test_instructions
    [ MIcomment ["comment in seq"]
    ; MIswap
    ];
  [%expect {|
    { SWAP }
    ->
    { SWAP } |}]

let%expect_test "is_iter_cons_match_nil_nil" = 
  test_instructions
    [ MI0 (Nil mt_int)
    ; MIdig 1
    ; MI0 (Nil mt_int)
    ; MIdig 1
    ; MIiter { instr = MI2 Cons }
    ; MIiter { instr = MI2 Cons }
    ];
  [%expect {|
    { NIL int ; DIG 1 ; NIL int ; DIG 1 ; ITER { CONS } ; ITER { CONS } }
    ->
    { NIL int ; NIL int ; DIG 2 ; ITER { CONS } ; ITER { CONS } } |}]

let%expect_test "is_iter_cons_with_one_match_only" =
  test_instructions
    [ MI0 (Nil mt_string)
    ; MIdig 1
    ; MIiter { instr = MI2 Cons }
    ; MIiter { instr = MIdrop }
    ];
  [%expect {|
    { NIL string ; DIG 1 ; ITER { CONS } ; ITER { DROP } }
    ->
    { NIL string ; SWAP ; ITER { CONS } ; ITER { DROP } } |}]

let%expect_test "main_merge_consecutive_comments" =
  test_instructions
    [ MIcomment ["comment 1"]
    ; MIcomment ["comment 2"]
    ; MIcomment ["comment 3"]
    ; MIpush (mt_int, MLiteral.small_int 42)
    ];
  [%expect {|
    { PUSH int 42 }
    ->
    { PUSH int 42 } |}]

let%expect_test "main_flatten_sequences" =
  test_instructions
    [ MIseq
        [ mk (MIpush (mt_nat, MLiteral.small_int 10))
        ; mk MIdrop
        ; mk (MIpush (mt_nat, MLiteral.small_int 20))
        ]
    ; MIdrop
    ];
  [%expect {|
    { PUSH nat 10 ; DROP ; PUSH nat 20 ; DROP }
    ->
    {} |}]

let%expect_test "main_superfluous_swap" =
  test_instructions
    [ MIdup 1
    ; MIdig 1
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "post-swap")
    ];
  [%expect {|
    { DUP ; DIG 1 ; DROP ; PUSH string "post-swap" }
    ->
    { PUSH string "post-swap" } |}]

let%expect_test "main_swap_consecutive_pushes_with_dig1" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "a")
    ; MIpush (mt_string, MLiteral.string "b")
    ; MIdig 1
    ; MIdrop
    ];
  [%expect {|
    { PUSH string "a" ; PUSH string "b" ; DIG 1 ; DROP }
    ->
    { PUSH string "b" } |}]

let%expect_test "main_instr_before_push_failwith" =
  test_instructions
    [ MIswap
    ; MIpush (mt_string, MLiteral.string "some error")
    ; MI1_fail Failwith
    ; MIpush (mt_int, MLiteral.small_int 111)  
    ];
  [%expect {|
    { SWAP ; PUSH string "some error" ; FAILWITH ; PUSH int 111 }
    ->
    { PUSH string "some error" ; FAILWITH } |}]

let%expect_test "main_pushy_instr_drop" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 123)
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "continuation")
    ];
  [%expect {|
    { PUSH nat 123 ; DROP ; PUSH string "continuation" }
    ->
    { PUSH string "continuation" } |}]

let%expect_test "main_one_to_one_instr_drop" =
  test_instructions
    [ MI1 Not
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 42)
    ];
  [%expect {|
    { NOT ; DROP ; PUSH int 42 }
    ->
    { DROP ; PUSH int 42 } |}]

let%expect_test "main_two_to_one_instr_drop" =
  test_instructions
    [ MI2 Compare
    ; MIdrop
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { COMPARE ; DROP ; PUSH bool True }
    ->
    { DROP 2 ; PUSH bool True } |}]

let%expect_test "main_two_to_one_instr_double_drop" =
  test_instructions
    [ MI2 Compare
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "hello")
    ];
  [%expect {|
    { COMPARE ; DROP ; DROP ; PUSH string "hello" }
    ->
    { DROP 3 ; PUSH string "hello" } |}]

let%expect_test "main_merge_consecutive_DIP" =
   test_instructions
     [ MIdip
         { instr =
             MIseq
               [ mk (MIpush (mt_nat, MLiteral.small_int 10))
               ; mk (MIdrop)
               ]
         }
     ; MIdip
         { instr =
             MIseq
               [ mk (MIpush (mt_string, MLiteral.string "hello"))
               ; mk (MI1 Not)
               ]
         }
     ; MIdrop
     ];
   [%expect {|
     { DIP { PUSH nat 10 ; DROP } ;
       DIP { PUSH string "hello" ; NOT } ;
       DROP }
     ->
     { DROP ; PUSH string "hello" ; NOT } |}]
 

let%expect_test "main_DIP_with_DROP_only" =
   test_instructions
     [ MIdip { instr = MIdrop }
     ; MIpush (mt_int, MLiteral.small_int 999)
     ];
   [%expect {|
     { DIP { DROP } ; PUSH int 999 }
     ->
     { SWAP ; DROP ; PUSH int 999 } |}]
 
 let%expect_test "main_DUP1_DIP_with_1to1_instr" =
   test_instructions
     [ MIdup 1
     ; MIdip
         { instr =
             MIseq
               [ mk (MI1 Not)
               ]
         }
     ; MIdrop
     ];
   [%expect {|
     { DUP ; DIP { NOT } ; DROP }
     ->
     { NOT } |}]
 
 let%expect_test "main_DIP_then_DROP" =
   test_instructions
     [ MIdip { instr = MIseq [ mk (MI2 Add) ] }
     ; MIdrop
     ; MIpush (mt_string, MLiteral.string "unused")
     ];
   [%expect {|
     { DIP { ADD } ; DROP ; PUSH string "unused" }
     ->
     { DROP ; ADD ; PUSH string "unused" } |}]
 
 let%expect_test "main_loop_push_bool_false" =
   test_instructions
     [ MIpush (mt_bool, MLiteral.bool false)
     ; MIloop
         { instr =
             MIseq
               [ mk (MI2 Mul)
               ; mk (MIdrop)
               ]
         }
     ; MIpush (mt_int, MLiteral.small_int 100)
     ];
   [%expect {|
     { PUSH bool False ; LOOP { MUL ; DROP } ; PUSH int 100 }
     ->
     { PUSH int 100 } |}]
 
 let%expect_test "main_loop_left_push_right" =
   test_instructions
     [ MI1 (Right (None, None, mt_string)) 
     ; MIloop_left
         { instr = 
             MIseq
               [ mk (MIdrop)
               ; mk (MIpush (mt_bool, MLiteral.bool true))
               ]
         }
     ; MIpush (mt_bool, MLiteral.bool false)
     ];
   [%expect {|
     { RIGHT string ; LOOP_LEFT { DROP ; PUSH bool True } ; PUSH bool False }
     ->
     { PUSH bool False } |}]

 let%expect_test "main_push_pair_literal" =
   test_instructions
     [ MIpush (mt_nat, MLiteral.small_int 11)
     ; MIpush (mt_string, MLiteral.string "s")
     ; MI2 (Pair (None, None))
     ; MIdrop
     ];
   [%expect {|
     { PUSH nat 11 ; PUSH string "s" ; PAIR ; DROP }
     ->
     {} |}]
 
 let%expect_test "main_push_literal_then_Some" =
   test_instructions
     [ MIpush (mt_int, MLiteral.small_int 10)
     ; MI1 Some_
     ; MIdrop
     ];
   [%expect {|
     { PUSH int 10 ; SOME ; DROP }
     ->
     {} |}]
 
let%expect_test "main_push_and_left" =
   test_instructions
     [ MIpush (mt_nat, MLiteral.small_int 123)
     ; MI1 (Left (None, None, mt_string))
     ; MIdrop
     ];
   [%expect {|
     { PUSH nat 123 ; LEFT string ; DROP }
     ->
     {} |}]
 
let%expect_test "main_push_and_right" =
   test_instructions
     [ MIpush (mt_string, MLiteral.string "hello")
     ; MI1 (Right (None, None, mt_nat))
     ; MIdrop
     ];
   [%expect {|
     { PUSH string "hello" ; RIGHT nat ; DROP }
     ->
     {} |}]

let%expect_test "main_two_one_instr_drop_drop" =
  test_instructions
    [ MI2 Compare
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 42)
    ];
  [%expect {|
    { COMPARE ; DROP ; DROP ; PUSH int 42 }
    ->
    { DROP 3 ; PUSH int 42 } |}]

let%expect_test "main_two_one_instr_drop_dig_drop" =
  test_instructions
    [ MI2 (Pair (None, None))
    ; MIdrop
    ; MIdig 3
    ; MIdrop
    ; MIpush (mt_bool, MLiteral.bool false)
    ];
  [%expect {|
    { PAIR ; DROP ; DIG 3 ; DROP ; PUSH bool False }
    ->
    { DROP 2 ; DIG 3 ; DROP ; PUSH bool False } |}]

let%expect_test "main_three_one_harmless_instr_drop" =
  test_instructions
    [ MI3 Slice 
    ; MIdrop
    ; MIpush (mt_nat, MLiteral.small_int 987)
    ];
  [%expect {|
    { SLICE ; DROP ; PUSH nat 987 }
    ->
    { DROP 3 ; PUSH nat 987 } |}]

  let%expect_test "main_remove_dip_drop" =
  test_instructions
    [ MIdip { instr = MIdrop }
    ; MIpush (mt_string, MLiteral.string "after dip+drop")
    ];
  [%expect {|
    { DIP { DROP } ; PUSH string "after dip+drop" }
    ->
    { SWAP ; DROP ; PUSH string "after dip+drop" } |}]

let%expect_test "main_remove_dip_empty_seq" =
  test_instructions
    [ MIdip { instr = MIseq [] }
    ; MIpush (mt_string, MLiteral.string "another instruction")
    ];
  [%expect {|
    { DIP {} ; PUSH string "another instruction" }
    ->
    { PUSH string "another instruction" } |}]

let%expect_test "main_merge_nested_dip" =
  test_instructions
    [ MIdip { instr = MIseq [ mk (MIpush (mt_nat, MLiteral.small_int 1)) ] }
    ; MIdip { instr = MIseq [ mk (MIpush (mt_nat, MLiteral.small_int 2)) ] }
    ; MIpush (mt_nat, MLiteral.small_int 3)
    ];
  [%expect {|
    { DIP { PUSH nat 1 } ; DIP { PUSH nat 2 } ; PUSH nat 3 }
    ->
    { DIP { PUSH nat 1 ; PUSH nat 2 } ; PUSH nat 3 } |}]

let%expect_test "main_dup1_dip_one_to_one_instr" =
  test_instructions
    [ MIdup 1
    ; MIdip { instr = MI1 Not }
    ; MIpush (mt_string, MLiteral.string "continuation")
    ];
  [%expect {|
    { DUP ; DIP { NOT } ; PUSH string "continuation" }
    ->
    { DUP ; NOT ; SWAP ; PUSH string "continuation" } |}]

let%expect_test "main_push_literal_some" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 123)
    ; MI1 Some_
    ; MIpush (mt_string, MLiteral.string "after some")
    ];
  [%expect {|
    { PUSH nat 123 ; SOME ; PUSH string "after some" }
    ->
    { PUSH nat 123 ; SOME ; PUSH string "after some" } |}]

let%expect_test "main_push_literal_left" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 42)
    ; MI1 (Left (Some "annot_left", None, mt_string))
    ; MIdrop
    ];
  [%expect {|
    { PUSH nat 42 ; LEFT %annot_left string ; DROP }
    ->
    {} |}]

let%expect_test "main_push_literal_right" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "right side")
    ; MI1 (Right (None, Some "annot_right", mt_nat))
    ; MIdrop
    ];
  [%expect{|
    { PUSH string "right side" ; RIGHT %annot_right nat ; DROP }
    ->
    {} |}]

let%expect_test "main_push_push_pair" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 10)
    ; MIpush (mt_string, MLiteral.string "xyz")
    ; MI2 (Pair (Some "fst", Some "snd"))
    ; MIdrop
    ];
  [%expect {|
    { PUSH int 10 ; PUSH string "xyz" ; PAIR %fst %snd ; DROP }
    ->
    {} |}]

let%expect_test "main_push_push_pair_with_comment" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "ab")
    ; MIcomment ["some annotation"]
    ; MIpush (mt_nat, MLiteral.small_int 66)
    ; MI2 (Pair (None, None))
    ];
  [%expect {|
    { PUSH string "ab" ; PUSH nat 66 ; PAIR }
    ->
    { PUSH string "ab" ; PUSH nat 66 ; PAIR } |}]

let%expect_test "main_push_pair_then_mifield_a" =
  test_instructions
    [ MIpush
        ( mt_pair mt_int mt_bool
        , MLiteral.pair (MLiteral.small_int 123) (MLiteral.bool true) )
    ; MIfield [ A; D ]
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { PUSH (pair int bool) (Pair 123 True) ; CAR ; CDR ; PUSH string "rest" }
    ->
    { PUSH int 123 ; CDR ; PUSH string "rest" } |}]

let%expect_test "main_push_pair_then_mifield_d" =
  test_instructions
    [ MIpush
        ( mt_pair mt_nat mt_string
        , MLiteral.pair (MLiteral.small_int 999) (MLiteral.string "D-part") )
    ; MIfield [ D ]
    ; MIpush (mt_bool, MLiteral.bool false)
    ];
  [%expect {|
    { PUSH (pair nat string) (Pair 999 "D-part") ; CDR ; PUSH bool False }
    ->
    { PUSH string "D-part" ; PUSH bool False } |}]

let%expect_test "main_nil_push_cons_single_element_list" =
  test_instructions
    [ MI0 (Nil mt_string)
    ; MIpush (mt_string, MLiteral.string "hello")
    ; MI2 Cons
    ; MIpush (mt_string, MLiteral.string "post")
    ];
  [%expect {|
    { NIL string ; PUSH string "hello" ; CONS ; PUSH string "post" }
    ->
    { NIL string ; PUSH string "hello" ; CONS ; PUSH string "post" } |}]

let%expect_test "main_double_nil_iter_cons_pattern" =
  test_instructions
    [ MI0 (Nil mt_nat)
    ; MIdig 1
    ; MI0 (Nil mt_nat)
    ; MIdig 1
    ; MIiter { instr = MI2 Cons } 
    ; MIiter { instr = MI2 Cons }  
    ; MIpush (mt_nat, MLiteral.small_int 123)
    ];
  [%expect {|
    { NIL nat ;
      DIG 1 ;
      NIL nat ;
      DIG 1 ;
      ITER { CONS } ;
      ITER { CONS } ;
      PUSH nat 123 }
    ->
    { NIL nat ;
      NIL nat ;
      DIG 2 ;
      ITER { CONS } ;
      ITER { CONS } ;
      PUSH nat 123 } |}]

let%expect_test "main_push_seq_then_push_elem_cons" =
  test_instructions
    [ MIpush (mt_list mt_string, MLiteral.list [MLiteral.string "world"])
    ; MIpush (mt_string, MLiteral.string "hello")
    ; MI2 Cons
    ; MIdrop
    ];
  [%expect {|
    { PUSH (list string) { "world" } ; PUSH string "hello" ; CONS ; DROP }
    ->
    {} |}]

let%expect_test "main_push_bool_not" =
  test_instructions
    [ MIpush (mt_bool, MLiteral.bool true)
    ; MI1 Not
    ; MIpush (mt_string, MLiteral.string "done")
    ];
  [%expect {|
    { PUSH bool True ; NOT ; PUSH string "done" }
    ->
    { PUSH bool False ; PUSH string "done" } |}]

let%expect_test "main_push_bool_and" =
  test_instructions
    [ MIpush (mt_bool, MLiteral.bool true)
    ; MIpush (mt_bool, MLiteral.bool false)
    ; MI2 And
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { PUSH bool True ; PUSH bool False ; AND ; PUSH string "rest" }
    ->
    { PUSH bool False ; PUSH string "rest" } |}]

let%expect_test "main_push_bool_or" =
  test_instructions
    [ MIpush (mt_bool, MLiteral.bool false)
    ; MIpush (mt_bool, MLiteral.bool true)
    ; MI2 Or
    ; MIdrop
    ];
  [%expect {|
    { PUSH bool False ; PUSH bool True ; OR ; DROP }
    ->
    {} |}]

let%expect_test "main_get_and_update_drop" =
  test_instructions
    [ MI3 Get_and_update
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 999)
    ];
  [%expect {|
    { GET_AND_UPDATE ; DROP ; PUSH int 999 }
    ->
    { UPDATE ; PUSH int 999 } |}]

let%expect_test "main_pair_then_mifield_d" =
  test_instructions
    [ MI2 (Pair (None, None))
    ; MIfield [ D; A ]
    ; MIpush (mt_string, MLiteral.string "more instructions")
    ];
  [%expect {|
    { PAIR ; CDR ; CAR ; PUSH string "more instructions" }
    ->
    { DROP ; CAR ; PUSH string "more instructions" } |}]

let%expect_test "main_pair_comment_mifield_d" =
  test_instructions
    [ MI2 (Pair (Some "fst", Some "snd"))
    ; MIcomment ["some comment"]
    ; MIfield [ D ]
    ; MIdrop
    ];
  [%expect {|
    { PAIR %fst %snd ; CDR ; DROP }
    ->
    { DROP 2 } |}]

let%expect_test "main_pair_then_mifield_a" =
  test_instructions
    [ MI2 (Pair (None, None))
    ; MIfield [ A; D ]
    ; MIpush (mt_string, MLiteral.string "rest code")
    ];
  [%expect {|
    { PAIR ; CAR ; CDR ; PUSH string "rest code" }
    ->
    { SWAP ; DROP ; CDR ; PUSH string "rest code" } |}]

let%expect_test "main_pair_comment_mifield_a" =
  test_instructions
    [ MI2 (Pair (Some "x", Some "y"))
    ; MIcomment ["comment inside"]
    ; MIfield [ A ]
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { PAIR %x %y ; CAR ; PUSH bool True }
    ->
    { SWAP ; DROP ; PUSH bool True } |}]

let%expect_test "main_mifield_concat" =
  test_instructions
    [ MIfield [ A; D ]
    ; MIfield [ A ]
    ; MIpush (mt_nat, MLiteral.small_int 99)
    ];
  [%expect {|
    { CAR ; CDR ; CAR ; PUSH nat 99 }
    ->
    { CAR ; GET 3 ; PUSH nat 99 } |}]

let%expect_test "main_mifield_empty" =
  test_instructions
    [ MIfield []
    ; MIpush (mt_nat, MLiteral.small_int 2023)
    ];
  [%expect {|
    { PUSH nat 2023 }
    ->
    { PUSH nat 2023 } |}]

let%expect_test "main_push_false_loop" =
  test_instructions
    [ MIpush (mt_bool, MLiteral.bool false)
    ; MIloop { instr = MI2 Sub }
    ; MIdrop
    ];
  [%expect {|
    { PUSH bool False ; LOOP { SUB } ; DROP }
    ->
    { DROP } |}]

let%expect_test "main_push_right_loop_left" =
  test_instructions
    [ MI1 (Right (None, None, mt_int))
    ; MIloop_left { instr = MIswap }
    ; MIpush (mt_string, MLiteral.string "continuation")
    ];
  [%expect {|
    { RIGHT int ; LOOP_LEFT { SWAP } ; PUSH string "continuation" }
    ->
    { PUSH string "continuation" } |}]

let%expect_test "main_dup1_dip_dup1" =
  test_instructions
    [ MIdup 1
    ; MIdip { instr = MIdup 1 }
    ; MIpush (mt_nat, MLiteral.small_int 77)
    ];
  [%expect {|
    { DUP ; DIP { DUP } ; PUSH nat 77 }
    ->
    { DUP ; DUP ; PUSH nat 77 } |}]

let%expect_test "main_dup1_dip_drop" =
  test_instructions
    [ MIdup 1
    ; MIdip { instr = MIdrop }
    ; MIpush (mt_string, MLiteral.string "post dip drop")
    ];
  [%expect {|
    { DUP ; DIP { DROP } ; PUSH string "post dip drop" }
    ->
    { PUSH string "post dip drop" } |}]

let%expect_test "main_dig1_commutative_binop" =
  test_instructions
    [ MIdig 1
    ; MI2 Add
    ; MIdrop
    ];
  [%expect {|
    { DIG 1 ; ADD ; DROP }
    ->
    { ADD ; DROP } |}]

let%expect_test "main_dig1_compare_eq" =
  test_instructions
    [ MIdig 1
    ; MI2 Compare
    ; MI1 Eq
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DIG 1 ; COMPARE ; EQ ; PUSH string "rest" }
    ->
    { COMPARE ; EQ ; PUSH string "rest" } |}]

let%expect_test "main_dig1_compare_neq" =
  test_instructions
    [ MIdig 1
    ; MI2 Compare
    ; MI1 Neq
    ; MIdrop
    ];
  [%expect {|
    { DIG 1 ; COMPARE ; NEQ ; DROP }
    ->
    { DROP 2 } |}]

let%expect_test "main_eq_not_to_neq" =
  test_instructions
    [ MI1 Eq
    ; MI1 Not
    ; MIpush (mt_int, MLiteral.small_int 200)
    ];
  [%expect {|
    { EQ ; NOT ; PUSH int 200 }
    ->
    { NEQ ; PUSH int 200 } |}]

let%expect_test "main_neq_not_to_eq" =
  test_instructions
    [ MI1 Neq
    ; MI1 Not
    ; MIpush (mt_int, MLiteral.small_int 111)
    ];
  [%expect {|
    { NEQ ; NOT ; PUSH int 111 }
    ->
    { EQ ; PUSH int 111 } |}]

let%expect_test "main_dig1_compare_lt" =
  test_instructions
    [ MIdig 1
    ; MI2 Compare
    ; MI1 Lt
    ; MIdrop
    ];
  [%expect {|
    { DIG 1 ; COMPARE ; LT ; DROP }
    ->
    { DROP 2 } |}]

let%expect_test "main_dig1_compare_gt" =
  test_instructions
    [ MIdig 1
    ; MI2 Compare
    ; MI1 Gt
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { DIG 1 ; COMPARE ; GT ; PUSH bool True }
    ->
    { COMPARE ; LT ; PUSH bool True } |}]

let%expect_test "main_bubble_up_drop_push_dig1_drop" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "hello")
    ; MIdig 1
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 42)
    ];
  [%expect {|
    { PUSH string "hello" ; DIG 1 ; DROP ; PUSH int 42 }
    ->
    { DROP ; PUSH string "hello" ; PUSH int 42 } |}]

let%expect_test "main_bubble_up_drop_dig1_double_drop" =
  test_instructions
    [ MIdig 1
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "done")
    ];
  [%expect {|
    { DIG 1 ; DROP ; DROP ; PUSH string "done" }
    ->
    { DROP 2 ; PUSH string "done" } |}]

let%expect_test "main_bubble_up_drop_dip" =
  test_instructions
    [ MIdip { instr = MIseq [ mk (MIpush (mt_nat, MLiteral.small_int 99)) ] }
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest code")
    ];
  [%expect {|
    { DIP { PUSH nat 99 } ; DROP ; PUSH string "rest code" }
    ->
    { DROP ; PUSH nat 99 ; PUSH string "rest code" } |}]

let%expect_test "main_bubble_up_dip_after_one_to_one_mono" =
  test_instructions
    [ MI1 Not
    ; MIdip { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 123)) ] }
    ; MIdrop
    ];
  [%expect {|
    { NOT ; DIP { PUSH int 123 } ; DROP }
    ->
    { DROP ; PUSH int 123 } |}]

let%expect_test "main_bubble_up_dip_push" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "pushed")
    ; MIdip { instr = MIseq [ mk (MIpush (mt_bool, MLiteral.bool true)) ] }
    ; MIpush (mt_int, MLiteral.small_int 10)
    ];
  [%expect {|
    { PUSH string "pushed" ; DIP { PUSH bool True } ; PUSH int 10 }
    ->
    { PUSH bool True ; PUSH string "pushed" ; PUSH int 10 } |}]

let%expect_test "main_bubble_up_swap_push_mono" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 777)
    ; MIdig 1
    ; MI1 Not 
    ; MIdrop
    ];
  [%expect {|
    { PUSH nat 777 ; DIG 1 ; NOT ; DROP }
    ->
    { DROP ; PUSH nat 777 } |}]

let%expect_test "main_double_swap_bubble" =
  test_instructions
    [ MI1 Not
    ; MIdig 1
    ; MI1 Neg
    ; MIdig 1
    ; MIpush (mt_string, MLiteral.string "continuation")
    ];
  [%expect {|
    { NOT ; DIG 1 ; NEG ; DIG 1 ; PUSH string "continuation" }
    ->
    { SWAP ; NEG ; SWAP ; NOT ; PUSH string "continuation" } |}]

let%expect_test "main_dig_dug_same_n_with_comment" =
  test_instructions
    [ MIdig 2
    ; MIcomment ["some comment"]
    ; MIdug 2
    ; MIpush (mt_int, MLiteral.small_int 42)
    ];
  [%expect {|
    { DIG 2 ; DUG 2 ; PUSH int 42 }
    ->
    { PUSH int 42 } |}]

let%expect_test "main_multiple_dig_drops" =
  test_instructions
    [ MIdig 3
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DIG 3 ; DIG 2 ; DROP ; PUSH string "rest" }
    ->
    { SWAP ; DROP ; DIG 2 ; PUSH string "rest" } |}]

let%expect_test "main_push_dig_drop_n_greater_one" =
  test_instructions
    [ MIpush (mt_bool, MLiteral.bool true)
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "foo")
    ];
  [%expect {|
    { PUSH bool True ; DIG 2 ; DROP ; PUSH string "foo" }
    ->
    { SWAP ; DROP ; PUSH bool True ; PUSH string "foo" } |}]

let%expect_test "main_dup1_dig_drop" =
  test_instructions
    [ MIdup 1
    ; MIdig 3
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { DUP ; DIG 3 ; DROP ; DROP }
    ->
    { DIG 2 ; DROP } |}]

let%expect_test "main_dup_k_dig_n_drop_variants" =
  test_instructions
    [ MIdup 2
    ; MIdig 3
    ; MIdrop
    ; MIpush (mt_unit, MLiteral.unit)
    ];
  [%expect {|
    { DUP 2 ; DIG 3 ; DROP ; PUSH unit Unit }
    ->
    { DIG 2 ; DROP ; DUP 2 ; UNIT } |}]

let%expect_test "main_dup_k_dig_n_eq" =
  test_instructions
    [ MIdup 3
    ; MIdig 3
    ; MIpush (mt_nat, MLiteral.small_int 999)
    ];
  [%expect {|
    { DUP 3 ; DIG 3 ; PUSH nat 999 }
    ->
    { DIG 2 ; DUP ; PUSH nat 999 } |}]

let%expect_test "main_dug_n1_mono_dig_n2" =
  test_instructions
    [ MIdug 3
    ; MI1 Not
    ; MIdig 3
    ; MIpush (mt_string, MLiteral.string "done")
    ];
  [%expect {|
    { DUG 3 ; NOT ; DIG 3 ; PUSH string "done" }
    ->
    { SWAP ; NOT ; SWAP ; PUSH string "done" } |}]

let%expect_test "main_dig_n_dig1_mono_dig1" =
  test_instructions
    [ MIdig 2
    ; MIdig 1
    ; MI1 Neg
    ; MIdig 1
    ; MIdrop
    ];
  [%expect {|
    { DIG 2 ; DIG 1 ; NEG ; DIG 1 ; DROP }
    ->
    { DIG 2 ; DROP ; NEG } |}]

let%expect_test "main_dug_midig_drop_mixed" =
  test_instructions
    [ MIdug 3
    ; MIdig 1
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest code")
    ];
  [%expect {|
    { DUG 3 ; DIG 1 ; DROP ; PUSH string "rest code" }
    ->
    { DIG 2 ; DROP ; DUG 2 ; PUSH string "rest code" } |}]

let%expect_test "main_2_1_binop_dig_drop" =
  test_instructions
    [ MI2 Concat2
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "final")
    ];
  [%expect {|
    { CONCAT ; DIG 2 ; DROP ; PUSH string "final" }
    ->
    { CONCAT ; DIG 2 ; DROP ; PUSH string "final" } |}]

let%expect_test "main_1_2_instr_dig_drop" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdig 3
    ; MIdrop
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect{|
    { UNPAIR ; DIG 3 ; DROP ; PUSH bool True }
    ->
    { DIG 2 ; DROP ; UNPAIR ; PUSH bool True } |}]

let%expect_test "main_1_2_instr_dig1_drop_dig_n_drop" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdig 1
    ; MIdrop
    ; MIdig 2
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { UNPAIR ; DIG 1 ; DROP ; DIG 2 ; DROP ; DROP }
    ->
    { DIG 2 ; DROP 2 } |}]

let%expect_test "main_1_2_instr_drop_drop" =
  test_instructions
    [ MIpairn 2
    ; MIdig 1
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { PAIR 2 ; DIG 1 ; DROP ; DROP ; PUSH string "rest" }
    ->
    { DROP 3 ; PUSH string "rest" } |}]

let%expect_test "main_1_2_instr_drop_drop_second" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 42)
    ];
  [%expect {|
    { UNPAIR ; DROP ; DROP ; PUSH int 42 }
    ->
    { DROP ; PUSH int 42 } |}]

let%expect_test "main_1_2_instr_dig_drop_again" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdig 3
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { UNPAIR ; DIG 3 ; DROP ; DROP }
    ->
    { DIG 2 ; DROP ; CDR } |}]

let%expect_test "main_pair_unpair_cancel" =
  test_instructions
    [ MI2 (Pair (None, None))
    ; MIunpair [true; true]
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { PAIR ; UNPAIR ; PUSH string "rest" }
    ->
    { PUSH string "rest" } |}]

let%expect_test "main_mipairn_unpair_alltrue" =
  test_instructions
    [ MIpairn 3
    ; MIunpair [true; true; true]
    ; MIdrop
    ];
  [%expect {|
    { PAIR 3 ; UNPAIR 3 ; DROP }
    ->
    { PAIR 3 ; CDR ; UNPAIR } |}]

let%expect_test "main_pair_comment_unpair_alltrue" =
  test_instructions
    [ MI2 (Pair (Some "a", Some "b"))
    ; MIcomment ["some comment"]
    ; MIunpair [true; true]
    ; MIpush (mt_nat, MLiteral.small_int 999)
    ];
  [%expect {|
    { PAIR %a %b ; UNPAIR ; PUSH nat 999 }
    ->
    { PUSH nat 999 } |}]

let%expect_test "main_unpair_pair_cancel" =
  test_instructions
    [ MIunpair [true; true]
    ; MI2 (Pair (None, None))
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { UNPAIR ; PAIR ; PUSH bool True }
    ->
    { PUSH bool True } |}]

let%expect_test "main_read_ticket_drop" =
  test_instructions
    [ MI1 Read_ticket
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "done")
    ];
  [%expect {|
    { READ_TICKET ; DROP ; PUSH string "done" }
    ->
    { PUSH string "done" } |}]

let%expect_test "main_dup1_mifield_d_dig1_mifield_a" =
  test_instructions
    [ MIdup 1
    ; MIfield [ D ]
    ; MIdig 1
    ; MIfield [ A ]
    ; MIdrop
    ];
  [%expect {|
    { DUP ; CDR ; DIG 1 ; CAR ; DROP }
    ->
    { CDR } |}]

let%expect_test "main_dup1_mifield_a_dig1_mifield_d" =
  test_instructions
    [ MIdup 1
    ; MIfield [ A ]
    ; MIdig 1
    ; MIfield [ D ]
    ; MIpush (mt_unit, MLiteral.unit)
    ];
  [%expect {|
    { DUP ; CAR ; DIG 1 ; CDR ; PUSH unit Unit }
    ->
    { UNPAIR ; SWAP ; UNIT } |}]

let%expect_test "main_ternary_instr_midig_drop" =
  test_instructions
    [ MI3 Update
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { UPDATE ; DIG 2 ; DROP ; PUSH string "rest" }
    ->
    { DIG 4 ; DROP ; UPDATE ; PUSH string "rest" } |}]

let%expect_test "main_ternary_3_2_midig_drop" =
  test_instructions
    [ MI2 Split_ticket
    ; MIdig 2
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { SPLIT_TICKET ; DIG 2 ; DROP ; DROP }
    ->
    { DIG 3 ; DROP 3 } |}]

let%expect_test "main_comment_push_fail_reorder" =
  test_instructions
    [ MIcomment ["some comment"]
    ; MIpush (mt_string, MLiteral.string "error msg")
    ; MI1_fail Failwith
    ; MIdrop
    ];
  [%expect {|
    { PUSH string "error msg" ; FAILWITH ; DROP }
    ->
    { PUSH string "error msg" ; FAILWITH } |}]

let%expect_test "main_dup1_then_fail_redundant" =
  test_instructions
    [ MIdup 1
    ; MI1_fail Never
    ; MIpush (mt_nat, MLiteral.small_int 100)
    ];
  [%expect {|
    { DUP ; NEVER ; PUSH nat 100 }
    ->
    { NEVER } |}]

let%expect_test "main_dup1_then_failwith" =
  test_instructions
    [ MIdup 1
    ; MI1_fail Failwith
    ; MIdrop
    ];
  [%expect {|
    { DUP ; FAILWITH ; DROP }
    ->
    { FAILWITH } |}]

let%expect_test "main_dug_n_pair_exec_fail" =
  test_instructions
    [ MIdug 3
    ; MI2 (Pair (None, None))
    ; MI2 Exec
    ; MI1_fail Failwith
    ; MIpush (mt_string, MLiteral.string "unreachable")
    ];
  [%expect {|
    { DUG 3 ; PAIR ; EXEC ; FAILWITH ; PUSH string "unreachable" }
    ->
    { DROP ; PAIR ; EXEC ; FAILWITH } |}]

let%expect_test "main_dug_n_push_midig_k_pair_exec_fail" =
  test_instructions
    [ MIdug 3
    ; MIpush (mt_string, MLiteral.string "lazy error string")
    ; MIdig 3
    ; MI2 (Pair (None, None))
    ; MI2 Exec
    ; MI1_fail Failwith
    ; MIdrop
    ];
  [%expect {|
    { DUG 3 ;
      PUSH string "lazy error string" ;
      DIG 3 ;
      PAIR ;
      EXEC ;
      FAILWITH ;
      DROP }
    ->
    { DROP ; PUSH string "lazy error string" ; DIG 3 ; PAIR ; EXEC ; FAILWITH } |}]

let%expect_test "main_create_contract_dig_drop" =
  test_instructions
    [ MIcreate_contract
        { tparameter = (mt_nat, None)
        ; tstorage = mt_string
        ; code = { instr = MIdrop }
        ; views = []
        }
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "post-create")
    ];
  [%expect {|
    { CREATE_CONTRACT { parameter nat ; storage string ; code { DROP } } ;
      DIG 2 ;
      DROP ;
      PUSH string "post-create" }
    ->
    { DIG 3 ;
      DROP ;
      CREATE_CONTRACT { parameter nat ; storage string ; code { DROP } } ;
      PUSH string "post-create" } |}]

let%expect_test "main_create_contract_output_ad_hoc" =
  test_instructions
    [ MI2 (Pair (Some "fst", Some "snd"))
    ; MIcomment ["some comment"]
    ; MIdup 1
    ; MIfield [ A ]
    ; MI0 (Nil mt_operation)
    ; MIdig 1
    ; MI2 Cons
    ; MIcomment ["another comment"]
    ; MIdig 1
    ; MIfield [ D ]
    ; MIpush (mt_nat, MLiteral.small_int 404)
    ];
  [%expect {|
    { PAIR %fst %snd ;
      DUP ;
      CAR ;
      NIL operation ;
      DIG 1 ;
      CONS ;
      DIG 1 ;
      CDR ;
      PUSH nat 404 }
    ->
    { NIL operation ; SWAP ; CONS ; SWAP ; PUSH nat 404 } |}]

let%expect_test "main_comment_drop_reorder" =
  test_instructions
    [ MIcomment ["some cmt"]
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DROP ; PUSH string "rest" }
    ->
    { DROP ; PUSH string "rest" } |}]

let%expect_test "main_comment_dig1_reorder" =
  test_instructions
    [ MIcomment ["cc"]
    ; MIdig 1
    ; MIpush (mt_bool, MLiteral.bool false)
    ];
  [%expect {|
    { DIG 1 ; PUSH bool False }
    ->
    { SWAP ; PUSH bool False } |}]

let%expect_test "main_comment_dig_n_drop_reorder" =
  test_instructions
    [ MIcomment ["c"]
    ; MIdig 3
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { DIG 3 ; DROP ; DROP }
    ->
    { DIG 3 ; DROP 2 } |}]

let%expect_test "main_mono_dig_drop" =
  test_instructions
    [ MI1 Not
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_nat, MLiteral.small_int 999)
    ];
  [%expect {|
    { NOT ; DIG 2 ; DROP ; PUSH nat 999 }
    ->
    { DIG 2 ; DROP ; NOT ; PUSH nat 999 } |}]

let%expect_test "main_iter_cons_dig_drop" =
  test_instructions
    [ MIiter { instr = MI2 Cons }
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "done")
    ];
  [%expect {|
    { ITER { CONS } ; DIG 2 ; DROP ; PUSH string "done" }
    ->
    { DIG 3 ; DROP ; ITER { CONS } ; PUSH string "done" } |}]

let%expect_test "main_push_dig1_iter_cons_dig1_drop" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 123)
    ; MIdig 1
    ; MIiter { instr = MI2 Cons }
    ; MIdig 1
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { PUSH int 123 ;
      DIG 1 ;
      ITER { CONS } ;
      DIG 1 ;
      DROP ;
      PUSH string "rest" }
    ->
    { SWAP ; DROP ; PUSH int 123 ; SWAP ; ITER { CONS } ; PUSH string "rest" } |}]

let%expect_test "main_dug_n_drop" =
  test_instructions
    [ MIdug 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "post")
    ];
  [%expect {|
    { DUG 2 ; DROP ; PUSH string "post" }
    ->
    { SWAP ; DROP ; SWAP ; PUSH string "post" } |}]

let%expect_test "main_dup1_dug_n_drop" =
  test_instructions
    [ MIdup 1
    ; MIdug 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DUP ; DUG 2 ; DROP ; PUSH string "rest" }
    ->
    { SWAP ; PUSH string "rest" } |}]

let%expect_test "main_dup1_dip_dig1" =
  test_instructions
    [ MIdup 1
    ; MIdip { instr = MIdig 1 }
    ; MIdrop
    ];
  [%expect {|
    { DUP ; DIP { DIG 1 } ; DROP }
    ->
    { SWAP } |}]

let%expect_test "main_push_dig1_dup1_dug2" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 321)
    ; MIdig 1
    ; MIdup 1
    ; MIdug 2
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { PUSH nat 321 ; DIG 1 ; DUP ; DUG 2 ; PUSH string "rest" }
    ->
    { PUSH nat 321 ; DUP 2 ; PUSH string "rest" } |}]

let%expect_test "main_dup1_push_binop_dig1_drop" =
  test_instructions
    [ MIdup 1
    ; MIpush (mt_int, MLiteral.small_int 444)
    ; MI2 Add
    ; MIdig 1
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { DUP ; PUSH int 444 ; ADD ; DIG 1 ; DROP ; DROP }
    ->
    { PUSH int 444 ; ADD ; DROP } |}]

let%expect_test "main_complex_dig_dug_interplay" =
  test_instructions
    [ MIdig 2
    ; MIdig 1
    ; MIdup 1
    ; MIdug 1
    ; MIdrop
    ];
  [%expect {|
    { DIG 2 ; DIG 1 ; DUP ; DUG 1 ; DROP }
    ->
    { DIG 2 ; SWAP } |}]

let%expect_test "main_constant_folding_compare" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 3)
    ; MIpush (mt_int, MLiteral.small_int 10)
    ; MI2 Compare
    ; MIdrop
    ];
  [%expect {|
    { PUSH int 3 ; PUSH int 10 ; COMPARE ; DROP }
    ->
    {} |}]

let%expect_test "main_constant_folding_int_eq" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 0)
    ; MI1 Eq
    ; MIpush (mt_string, MLiteral.string "done")
    ];
  [%expect {|
    { PUSH int 0 ; EQ ; PUSH string "done" }
    ->
    { PUSH bool True ; PUSH string "done" } |}]

let%expect_test "main_constant_folding_int_gt" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 1)
    ; MI1 Gt
    ; MIdrop
    ];
  [%expect {|
    { PUSH int 1 ; GT ; DROP }
    ->
    {} |}]

let%expect_test "main_drop_before_push_failwith" =
  test_instructions
    [ MIdrop
    ; MIpush (mt_string, MLiteral.string "some error")
    ; MI1_fail Failwith
    ; MIpush (mt_nat, MLiteral.small_int 123)
    ];
  [%expect {|
    { DROP ; PUSH string "some error" ; FAILWITH ; PUSH nat 123 }
    ->
    { PUSH string "some error" ; FAILWITH } |}]


let%expect_test "main_dup2_field_d_dig2_field_a" =
  test_instructions
    [ MIdup 2
    ; MIfield [ D ]
    ; MIdig 2
    ; MIfield [ A ]
    ; MIpush (mt_string, MLiteral.string "post")
    ];
  [%expect {|
    { DUP 2 ; CDR ; DIG 2 ; CAR ; PUSH string "post" }
    ->
    { SWAP ; UNPAIR ; PUSH string "post" } |}]

let%expect_test "main_swap_push_dig1" =
  test_instructions
    [ MIswap
    ; MIpush (mt_bool, MLiteral.bool false)
    ; MIdig 1
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { SWAP ; PUSH bool False ; DIG 1 ; PUSH string "rest" }
    ->
    { PUSH bool False ; DIG 2 ; PUSH string "rest" } |}]

let%expect_test "main_two_dig_with_drops_n1_gt_n2" =
  test_instructions
    [ MIdig 3
    ; MIdrop
    ; MIdig 1
    ; MIdrop
    ; MIpush (mt_unit, MLiteral.unit)
    ];
  [%expect {|
    { DIG 3 ; DROP ; DIG 1 ; DROP ; PUSH unit Unit }
    ->
    { SWAP ; DROP ; DIG 2 ; DROP ; UNIT } |}]


let%expect_test "main_dig1_dup1_dug2" =
  test_instructions
    [ MIdig 1
    ; MIdup 1
    ; MIdug 2
    ; MIdrop
    ];
  [%expect {|
    { DIG 1 ; DUP ; DUG 2 ; DROP }
    ->
    {} |}]

let%expect_test "main_dup2_dig1_drop" =
  test_instructions
    [ MIdup 2
    ; MIdig 1
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DUP 2 ; DIG 1 ; DROP ; PUSH string "rest" }
    ->
    { DROP ; DUP ; PUSH string "rest" } |}]

let%expect_test "main_field_concatenation_second_test" =
  test_instructions
    [ MIfield [ A; D ]
    ; MIfield [ D; A ]
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { CAR ; CDR ; CDR ; CAR ; PUSH bool True }
    ->
    { CAR ; GET 5 ; PUSH bool True } |}]

let%expect_test "main_nat_neg" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 5)
    ; MI1 Neg
    ; MIdrop
    ];
  [%expect {|
    { PUSH nat 5 ; NEG ; DROP }
    ->
    {} |}]

let%expect_test "main_nat_int_neg" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 12)
    ; MI1 Int
    ; MI1 Neg
    ; MIdrop
    ];
  [%expect {|
    { PUSH nat 12 ; INT ; NEG ; DROP }
    ->
    {} |}]

let%expect_test "main_sub_rewriting_extended" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 10)
    ; MIdig 1
    ; MI2 Sub
    ; MIdrop
    ];
  [%expect {|
    { PUSH int 10 ; DIG 1 ; SUB ; DROP }
    ->
    { PUSH int -10 ; ADD ; DROP } |}]

let%expect_test "main_neg_dig1_add_to_sub" =
  test_instructions
    [ MI1 Neg
    ; MIdig 1
    ; MI2 Add
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { NEG ; DIG 1 ; ADD ; PUSH string "rest" }
    ->
    { NEG ; ADD ; PUSH string "rest" } |}]

let%expect_test "main_neg_dig1_sub_to_add" =
  test_instructions
    [ MI1 Neg
    ; MIdig 1
    ; MI2 Sub
    ; MIdrop
    ];
  [%expect {|
    { NEG ; DIG 1 ; SUB ; DROP }
    ->
    { ADD ; DROP } |}]

let%expect_test "main_push_t_l_dup1" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "dup me")
    ; MIdup 1
    ; MIpush (mt_bool, MLiteral.bool false)
    ];
  [%expect {|
    { PUSH string "dup me" ; DUP ; PUSH bool False }
    ->
    { PUSH string "dup me" ; DUP ; PUSH bool False } |}]

let%expect_test "main_lambda_dig1_exec" =
  test_instructions
    [ MIlambda (mt_int, mt_int, { instr = MI1 Neg })
    ; MIdig 1
    ; MI2 Exec
    ; MIpush (mt_string, MLiteral.string "post-lambda")
    ];
  [%expect {|
    { LAMBDA int int { NEG } ; DIG 1 ; EXEC ; PUSH string "post-lambda" }
    ->
    { NEG ; PUSH string "post-lambda" } |}]

let%expect_test "main_push_instr_dig1_exec" =
  let lambda_body = { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 123)) ] } in
  test_instructions
    [ MIpush
        ( mt_lambda mt_unit mt_int
        , MLiteral.instr lambda_body
        )
    ; MIdig 1
    ; MI2 Exec
    ; MIdrop
    ];
  [%expect {|
    { PUSH (lambda unit int) { PUSH int 123 } ; DIG 1 ; EXEC ; DROP }
    ->
    { PUSH (lambda unit int) { PUSH int 123 } ; SWAP ; EXEC ; DROP } |}]

let%expect_test "main_dig_n_dup1_dug_n_plus_1" =
  test_instructions
    [ MIdig 2
    ; MIdup 1
    ; MIdug 3
    ; MIdrop
    ];
  [%expect {|
    { DIG 2 ; DUP ; DUG 3 ; DROP }
    ->
    {} |}]

let%expect_test "main_dup_n_swap_drop" =
  test_instructions
    [ MIdup 3
    ; MIswap
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DUP 3 ; SWAP ; DROP ; PUSH string "rest" }
    ->
    { DROP ; DUP 2 ; PUSH string "rest" } |}]

let%expect_test "main_dup1_dup2" =
  test_instructions
    [ MIdup 1
    ; MIdup 2
    ; MIdrop
    ];
  [%expect {|
    { DUP ; DUP 2 ; DROP }
    ->
    { DUP } |}]

let%expect_test "main_dup1_dup1_f_push_dig3" =
  test_instructions
    [ MIdup 1
    ; MIdup 1
    ; MI1 Not  
    ; MIpush (mt_int, MLiteral.small_int 123)
    ; MIdig 3
    ; MIpush (mt_nat, MLiteral.small_int 77)
    ];
  [%expect {|
    { DUP ; DUP ; NOT ; PUSH int 123 ; DIG 3 ; PUSH nat 77 }
    ->
    { DUP ; NOT ; PUSH int 123 ; DUP 3 ; PUSH nat 77 } |}]

let%expect_test "main_self_none_address" =
  test_instructions
    [ MI0 (Self None)
    ; MI1 Address
    ; MIdrop
    ];
  [%expect {|
    { SELF ; ADDRESS ; DROP }
    ->
    {} |}]

  let%expect_test "main_1_2_instr_drop_drop" =
  test_instructions
    [ MIpairn 2 
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "continuation")
    ];
  [%expect {|
    { PAIR 2 ; DROP ; DROP ; PUSH string "continuation" }
    ->
    { DROP 3 ; PUSH string "continuation" } |}]

let%expect_test "main_1_2_instr_dig1_drop_drop" =
  test_instructions
    [ MIpairn 2 
    ; MIdig 1
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "done")
    ];
  [%expect {|
    { PAIR 2 ; DIG 1 ; DROP ; DROP ; PUSH string "done" }
    ->
    { DROP 3 ; PUSH string "done" } |}]

let%expect_test "final_two_one_instr_double_drop_again" =
  test_instructions
    [ MI2 (Pair (None, None))
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 10)
    ];
  [%expect {|
    { PAIR ; DROP ; DROP ; PUSH int 10 }
    ->
    { DROP 3 ; PUSH int 10 } |}]

let%expect_test "final_two_one_instr_drop_dig_drop_variant" =
  test_instructions
    [ MI2 Compare
    ; MIdrop
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "after pattern")
    ];
  [%expect {|
    { COMPARE ; DROP ; DIG 2 ; DROP ; PUSH string "after pattern" }
    ->
    { DROP 2 ; DIG 2 ; DROP ; PUSH string "after pattern" } |}]

let%expect_test "final_three_one_instr_drop" =
  test_instructions
    [ MI3 (Slice) 
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "some next instr")
    ];
  [%expect {|
    { SLICE ; DROP ; PUSH string "some next instr" }
    ->
    { DROP 3 ; PUSH string "some next instr" } |}]

let%expect_test "final_remove_dip_drop" =
  test_instructions
    [ MIdip { instr = MIdrop }
    ; MIpush (mt_nat, MLiteral.small_int 123)
    ];
  [%expect {|
    { DIP { DROP } ; PUSH nat 123 }
    ->
    { SWAP ; DROP ; PUSH nat 123 } |}]

let%expect_test "final_remove_dip_empty_seq" =
  test_instructions
    [ MIdip { instr = MIseq [] }
    ; MIdrop
    ];
  [%expect {|
    { DIP {} ; DROP }
    ->
    { DROP } |}]

let%expect_test "final_merge_nested_dip" =
  test_instructions
    [ MIdip { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 1)) ] }
    ; MIdip { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 2)) ] }
    ; MIdrop
    ];
  [%expect {|
    { DIP { PUSH int 1 } ; DIP { PUSH int 2 } ; DROP }
    ->
    { DROP ; PUSH int 1 ; PUSH int 2 } |}]

let%expect_test "final_push_literal_some" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 99)
    ; MI1 Some_
    ; MIdrop
    ];
  [%expect {|
    { PUSH nat 99 ; SOME ; DROP }
    ->
    {} |}]

let%expect_test "final_push_literal_left" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "left side")
    ; MI1 (Left (None, None, mt_bool))
    ; MIdrop
    ];
  [%expect {|
    { PUSH string "left side" ; LEFT bool ; DROP }
    ->
    {} |}]

let%expect_test "final_push_literal_right" =
  test_instructions
    [ MIpush (mt_bool, MLiteral.bool true)
    ; MI1 (Right (None, None, mt_nat))
    ; MIdrop
    ];
  [%expect {|
    { PUSH bool True ; RIGHT nat ; DROP }
    ->
    {} |}]

let%expect_test "final_push_push_pair" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 1)
    ; MIpush (mt_nat, MLiteral.small_int 2)
    ; MI2 (Pair (Some "fst", Some "snd"))
    ; MIdrop
    ];
  [%expect {|
    { PUSH int 1 ; PUSH nat 2 ; PAIR %fst %snd ; DROP }
    ->
    {} |}]

let%expect_test "final_push_pair_then_mifield_a" =
  test_instructions
    [ MIpush
        ( mt_pair mt_nat mt_int
        , MLiteral.pair (MLiteral.small_int 7) (MLiteral.small_int 77)
        )
    ; MIfield [ A ]
    ; MIdrop
    ];
  [%expect {|
    { PUSH (pair nat int) (Pair 7 77) ; CAR ; DROP }
    ->
    {} |}]

let%expect_test "final_nil_push_cons" =
  test_instructions
    [ MI0 (Nil mt_string)
    ; MIpush (mt_string, MLiteral.string "some str")
    ; MI2 Cons
    ; MIdrop
    ];
  [%expect {|
    { NIL string ; PUSH string "some str" ; CONS ; DROP }
    ->
    {} |}]

let%expect_test "final_push_false_loop" =
  test_instructions
    [ MIpush (mt_bool, MLiteral.bool false)
    ; MIloop { instr = MIswap }
    ; MIdrop
    ];
  [%expect {|
    { PUSH bool False ; LOOP { SWAP } ; DROP }
    ->
    { DROP } |}]

let%expect_test "final_right_loop_left" =
  test_instructions
    [ MI1 (Right (None, None, mt_string))
    ; MIloop_left { instr = MI1 Not }
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { RIGHT string ; LOOP_LEFT { NOT } ; PUSH string "rest" }
    ->
    { PUSH string "rest" } |}]

let%expect_test "final_dig1_commutative" =
  test_instructions
    [ MIdig 1
    ; MI2 Add
    ; MIdrop
    ];
  [%expect {|
    { DIG 1 ; ADD ; DROP }
    ->
    { ADD ; DROP } |}]

let%expect_test "final_dig1_compare_eq" =
  test_instructions
    [ MIdig 1
    ; MI2 Compare
    ; MI1 Eq
    ; MIdrop
    ];
  [%expect {|
    { DIG 1 ; COMPARE ; EQ ; DROP }
    ->
    { DROP 2 } |}]

let%expect_test "final_bubble_drop_push_dig1" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "bubble-up")
    ; MIdig 1
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 101)
    ];
  [%expect {|
    { PUSH string "bubble-up" ; DIG 1 ; DROP ; PUSH int 101 }
    ->
    { DROP ; PUSH string "bubble-up" ; PUSH int 101 } |}]

let%expect_test "final_bubble_dip" =
  test_instructions
    [ MI1 Neg
    ; MIdip { instr = MIseq [ mk (MIpush (mt_int, MLiteral.small_int 20)) ] }
    ; MIdrop
    ];
  [%expect {|
    { NEG ; DIP { PUSH int 20 } ; DROP }
    ->
    { DROP ; PUSH int 20 } |}]

let%expect_test "final_bubble_swap_push_mono" =
  test_instructions
    [ MIpush (mt_bool, MLiteral.bool true)
    ; MIdig 1
    ; MI1 Not
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { PUSH bool True ; DIG 1 ; NOT ; PUSH string "rest" }
    ->
    { NOT ; PUSH bool True ; SWAP ; PUSH string "rest" } |}]

let%expect_test "final_multiple_dig_reorder" =
  test_instructions
    [ MIdig 3
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DIG 3 ; DIG 2 ; DROP ; PUSH string "rest" }
    ->
    { SWAP ; DROP ; DIG 2 ; PUSH string "rest" } |}]

let%expect_test "final_push_dig_n_greater1" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "some-literal")
    ; MIdig 3
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { PUSH string "some-literal" ; DIG 3 ; DROP ; DROP }
    ->
    { DIG 2 ; DROP } |}]

let%expect_test "final_dup1_dig_n_drop" =
  test_instructions
    [ MIdup 1
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_unit, MLiteral.unit)
    ];
  [%expect {|
    { DUP ; DIG 2 ; DROP ; PUSH unit Unit }
    ->
    { SWAP ; DROP ; DUP ; UNIT } |}]

let%expect_test "final_one_to_two_instr_dig_n_drop" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdig 2
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { UNPAIR ; DIG 2 ; DROP ; DROP }
    ->
    { SWAP ; DROP ; CDR } |}]

let%expect_test "final_pair_unpair_cancel" =
  test_instructions
    [ MI2 (Pair (None, None))
    ; MIunpair [true; true]
    ; MIpush (mt_int, MLiteral.small_int 999)
    ];
  [%expect {|
    { PAIR ; UNPAIR ; PUSH int 999 }
    ->
    { PUSH int 999 } |}]

let%expect_test "final_read_ticket_drop" =
  test_instructions
    [ MI1 Read_ticket
    ; MIdrop
    ; MIpush (mt_bool, MLiteral.bool false)
    ];
  [%expect {|
    { READ_TICKET ; DROP ; PUSH bool False }
    ->
    { PUSH bool False } |}]

let%expect_test "final_ternary_midig_drop" =
  test_instructions
    [ MI3 Update
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "end")
    ];
  [%expect {|
    { UPDATE ; DIG 2 ; DROP ; PUSH string "end" }
    ->
    { DIG 4 ; DROP ; UPDATE ; PUSH string "end" } |}]

let%expect_test "final_ternary_3_2" =
  test_instructions
    [ MI2 Split_ticket 
    ; MIdig 2
    ; MIdrop
    ; MIdrop
    ];
  [%expect {|
    { SPLIT_TICKET ; DIG 2 ; DROP ; DROP }
    ->
    { DIG 3 ; DROP 3 } |}]

let%expect_test "final_comment_push_fail_reorder" =
  test_instructions
    [ MIcomment ["some cmt"]
    ; MIpush (mt_string, MLiteral.string "fail message")
    ; MI1_fail Failwith
    ; MIdrop
    ];
  [%expect {|
    { PUSH string "fail message" ; FAILWITH ; DROP }
    ->
    { PUSH string "fail message" ; FAILWITH } |}]

let%expect_test "final_dup1_then_never_fail" =
  test_instructions
    [ MIdup 1
    ; MI1_fail Never
    ; MIpush (mt_int, MLiteral.small_int 101)
    ];
  [%expect {|
    { DUP ; NEVER ; PUSH int 101 }
    ->
    { NEVER } |}]

let%expect_test "final_dup1_then_failwith" =
  test_instructions
    [ MIdup 1
    ; MI1_fail Failwith
    ; MIpush (mt_string, MLiteral.string "unreachable")
    ];
  [%expect {|
    { DUP ; FAILWITH ; PUSH string "unreachable" }
    ->
    { FAILWITH } |}]

let%expect_test "final_dug_n_pair_exec_failwith" =
  test_instructions
    [ MIdug 3
    ; MI2 (Pair (None, None))
    ; MI2 Exec
    ; MI1_fail Failwith
    ; MIpush (mt_string, MLiteral.string "post")
    ];
  [%expect {|
    { DUG 3 ; PAIR ; EXEC ; FAILWITH ; PUSH string "post" }
    ->
    { DROP ; PAIR ; EXEC ; FAILWITH } |}]

let%expect_test "final_dug_n_push_midig_k_pair_fail" =
  test_instructions
    [ MIdug 3
    ; MIpush (mt_string, MLiteral.string "lazy error msg")
    ; MIdig 3
    ; MI2 (Pair (None, None))
    ; MI2 Exec
    ; MI1_fail Failwith
    ; MIdrop
    ];
  [%expect {|
    { DUG 3 ;
      PUSH string "lazy error msg" ;
      DIG 3 ;
      PAIR ;
      EXEC ;
      FAILWITH ;
      DROP }
    ->
    { DROP ; PUSH string "lazy error msg" ; DIG 3 ; PAIR ; EXEC ; FAILWITH } |}]

let%expect_test "final_create_contract_dig_drop" =
  test_instructions
    [ MIcreate_contract
        { tparameter = (mt_string, None)
        ; tstorage = mt_nat
        ; code = { instr = MIdrop }
        ; views = []
        }
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "after create_contract")
    ];
  [%expect {|
    { CREATE_CONTRACT { parameter string ; storage nat ; code { DROP } } ;
      DIG 2 ;
      DROP ;
      PUSH string "after create_contract" }
    ->
    { DIG 3 ;
      DROP ;
      CREATE_CONTRACT { parameter string ; storage nat ; code { DROP } } ;
      PUSH string "after create_contract" } |}]

let%expect_test "final_create_contract_output_pattern" =
  test_instructions
    [ MI2 (Pair (None, None))
    ; MIcomment ["some cmt"]
    ; MIdup 1
    ; MIfield [ A ]
    ; MI0 (Nil mt_operation)
    ; MIdig 1
    ; MI2 Cons
    ; MIcomment ["another cmt"]
    ; MIdig 1
    ; MIfield [ D ]
    ; MIpush (mt_nat, MLiteral.small_int 808)
    ];
  [%expect {|
    { PAIR ;
      DUP ;
      CAR ;
      NIL operation ;
      DIG 1 ;
      CONS ;
      DIG 1 ;
      CDR ;
      PUSH nat 808 }
    ->
    { NIL operation ; SWAP ; CONS ; SWAP ; PUSH nat 808 } |}]

let%expect_test "final_comment_drop_reorder" =
  test_instructions
    [ MIcomment ["a comment"]
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 555)
    ];
  [%expect {|
    { DROP ; PUSH int 555 }
    ->
    { DROP ; PUSH int 555 } |}]

let%expect_test "final_comment_dig1" =
  test_instructions
    [ MIcomment ["some text"]
    ; MIdig 1
    ; MIpush (mt_nat, MLiteral.small_int 123)
    ];
  [%expect {|
    { DIG 1 ; PUSH nat 123 }
    ->
    { SWAP ; PUSH nat 123 } |}]

let%expect_test "final_comment_dig_drop" =
  test_instructions
    [ MIcomment ["cc"]
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DIG 2 ; DROP ; PUSH string "rest" }
    ->
    { DIG 2 ; DROP ; PUSH string "rest" } |}]

let%expect_test "final_ternary_arbitrary" =
  test_instructions
    [ MI2 (Split_ticket)
    ; MIdig 3
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 777)
    ];
  [%expect {|
    { SPLIT_TICKET ; DIG 3 ; DROP ; DROP ; PUSH int 777 }
    ->
    { DIG 4 ; DROP 3 ; PUSH int 777 } |}]

let%expect_test "final_constant_folding_add" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 5)
    ; MIpush (mt_int, MLiteral.small_int 7)
    ; MI2 Add
    ; MIdrop
    ];
  [%expect {|
    { PUSH int 5 ; PUSH int 7 ; ADD ; DROP }
    ->
    {} |}]

let%expect_test "final_constant_folding_sub" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 15)
    ; MIpush (mt_int, MLiteral.small_int 6)
    ; MI2 Sub
    ; MIdrop
    ];
  [%expect {|
    { PUSH int 15 ; PUSH int 6 ; SUB ; DROP }
    ->
    {} |}]

let%expect_test "final_constant_folding_mul" =
  test_instructions
    [ MIpush (mt_int, MLiteral.small_int 3)
    ; MIpush (mt_int, MLiteral.small_int 4)
    ; MI2 Mul
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { PUSH int 3 ; PUSH int 4 ; MUL ; PUSH bool True }
    ->
    { PUSH int 12 ; PUSH bool True } |}]

let%expect_test "final_constant_folding_neg_nat" =
  test_instructions
    [ MIpush (mt_nat, MLiteral.small_int 10)
    ; MI1 Neg
    ; MIdrop
    ];
  [%expect {|
    { PUSH nat 10 ; NEG ; DROP }
    ->
    {} |}]

let%expect_test "final_1_2_instr_dig1_double_drop" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdig 1
    ; MIdrop
    ; MIdrop
    ; MIpush (mt_unit, MLiteral.unit)
    ];
  [%expect {|
    { UNPAIR ; DIG 1 ; DROP ; DROP ; PUSH unit Unit }
    ->
    { DROP ; UNIT } |}]

let%expect_test "final_push_literal_dup1_second" =
  test_instructions
    [ MIpush (mt_string, MLiteral.string "duplicate me!")
    ; MIdup 1
    ; MIpush (mt_bool, MLiteral.bool false)
    ];
  [%expect {|
    { PUSH string "duplicate me!" ; DUP ; PUSH bool False }
    ->
    { PUSH string "duplicate me!" ; DUP ; PUSH bool False } |}]

let%expect_test "final_lambda_exec_inlining" =
  test_instructions
    [ MIlambda (mt_nat, mt_string, { instr = MI1 Not })
    ; MIdig 1
    ; MI2 Exec
    ; MIdrop
    ];
  [%expect {|
    { LAMBDA nat string { NOT } ; DIG 1 ; EXEC ; DROP }
    ->
    { DROP } |}]

let%expect_test "final_push_instr_exec_inlining" =
  let body = { instr = MIseq [ mk (MIpush (mt_string, MLiteral.string "internal")) ] } in
  test_instructions
    [ MIpush (mt_lambda mt_unit mt_string, MLiteral.instr body)
    ; MIdig 1
    ; MI2 Exec
    ; MIdrop
    ];
  [%expect {|
    { PUSH (lambda unit string) { PUSH string "internal" } ;
      DIG 1 ;
      EXEC ;
      DROP }
    ->
    { PUSH (lambda unit string) { PUSH string "internal" } ; SWAP ; EXEC ; DROP } |}]

let%expect_test "final_dig_n_dup1_dug_n_plus1_check" =
  test_instructions
    [ MIdig 2
    ; MIdup 1
    ; MIdug 3
    ; MIdrop
    ];
  [%expect {|
    { DIG 2 ; DUP ; DUG 3 ; DROP }
    ->
    {} |}]

let%expect_test "final_dup_n_swap_drop" =
  test_instructions
    [ MIdup 2
    ; MIswap
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "afterwards")
    ];
  [%expect {|
    { DUP 2 ; SWAP ; DROP ; PUSH string "afterwards" }
    ->
    { DROP ; DUP ; PUSH string "afterwards" } |}]

let%expect_test "final_dup1_dup2_againa" =
  test_instructions
    [ MIdup 1
    ; MIdup 2
    ; MIpush (mt_nat, MLiteral.small_int 444)
    ];
  [%expect {|
    { DUP ; DUP 2 ; PUSH nat 444 }
    ->
    { DUP ; DUP ; PUSH nat 444 } |}]

let%expect_test "final_dup1_dup1_f_push_dig3_again" =
  test_instructions
    [ MIdup 1
    ; MIdup 1
    ; MI1 Not
    ; MIpush (mt_nat, MLiteral.small_int 333)
    ; MIdig 3
    ; MIdrop
    ];
  [%expect {|
    { DUP ; DUP ; NOT ; PUSH nat 333 ; DIG 3 ; DROP }
    ->
    { DUP ; NOT ; PUSH nat 333 } |}]

let%expect_test "final_self_none_address_conversion" =
  test_instructions
    [ MI0 (Self None)
    ; MI1 Address
    ; MIdrop
    ];
  [%expect {|
    { SELF ; ADDRESS ; DROP }
    ->
    {} |}]

let%expect_test "lltz_specific_pre_dig_n_dropn_m_when_n_less_than_m" =
  test_instructions
    [ MIdig 1
    ; MIdropn 3
    ; MIpush (mt_string, MLiteral.string "after")
    ];
  [%expect {|
    { DIG 1 ; DROP 3 ; PUSH string "after" }
    ->
    { DROP 3 ; PUSH string "after" } |}]

let%expect_test "lltz_specific_pre_dug_n_dropn_m_when_n_less_than_m" =
  test_instructions
    [ MIdug 2
    ; MIdropn 4
    ; MIpush (mt_nat, MLiteral.small_int 123)
    ];
  [%expect {|
    { DUG 2 ; DROP 4 ; PUSH nat 123 }
    ->
    { DROP 4 ; PUSH nat 123 } |}]

let%expect_test "lltz_specific_pre_no_rewrite_when_dig_n_is_not_less_than_m" =
  test_instructions
    [ MIdig 3
    ; MIdropn 2 
    ; MIdrop
    ];
  [%expect {|
    { DIG 3 ; DROP 2 ; DROP }
    ->
    { DIG 3 ; DROP 3 } |}]

let%expect_test "lltz_specific_pre_no_rewrite_when_dug_n_is_not_less_than_m" =
  test_instructions
    [ MIdug 5
    ; MIdropn 5 
    ; MIpush (mt_string, MLiteral.string "no rewrite")
    ];
  [%expect {|
    { DUG 5 ; DROP 5 ; PUSH string "no rewrite" }
    ->
    { DUG 5 ; DROP 5 ; PUSH string "no rewrite" } |}]

let%expect_test "lltz_specific_push_option_some" =
  test_instructions
    [ MIpush
        ( mt_option mt_nat
        , MLiteral.some (MLiteral.small_int 42)
        )
    ; MIdrop
    ];
  [%expect {|
    { PUSH (option nat) (Some 42) ; DROP }
    ->
    {} |}]

let%expect_test "lltz_specific_push_option_none" =
  test_instructions
    [ MIpush
        ( mt_option mt_string
        , MLiteral.none
        )
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { PUSH (option string) None ; PUSH bool True }
    ->
    { NONE string ; PUSH bool True } |}]

let%expect_test "lltz_specific_list_single_element" =
  test_instructions
    [ MIpush
        ( mt_list mt_nat
        , MLiteral.list [MLiteral.small_int 999] )
    ; MIdig 1
    ];
  [%expect {|
    { PUSH (list nat) { 999 } ; DIG 1 }
    ->
    { NIL nat ; PUSH nat 999 ; CONS ; SWAP } |}]

let%expect_test "lltz_specific_list_single_element_more_complex" =
  test_instructions
    [ MIpush
        ( mt_list mt_string
        , MLiteral.list [MLiteral.string "only-element"] )
    ; MIdrop
    ];
  [%expect {|
    { PUSH (list string) { "only-element" } ; DROP }
    ->
    {} |}]

let%expect_test "lltz_specific_lambda_int_int_constant_hash" =
  test_instructions
    [ MIlambda
        ( mt_int
        , mt_int
        , { instr = MIConstant { literal = String "some-hash-here" } }
        )
    ; MIdrop
    ];
  [%expect {|
    { LAMBDA int int { constant "some-hash-here" } ; DROP }
    ->
    {} |}]

let%expect_test "lltz_specific_push_or_left" =
  test_instructions
    [ MIpush
        ( mt_or mt_string mt_nat
        , MLiteral.left (MLiteral.string "left part")
        )
    ; MIdig 1
    ];
  [%expect {|
    { PUSH (or string nat) (Left "left part") ; DIG 1 }
    ->
    { PUSH string "left part" ; LEFT nat ; SWAP } |}]

let%expect_test "lltz_specific_push_or_right" =
  test_instructions
    [ MIpush
        ( mt_or mt_bool mt_int
        , MLiteral.right (MLiteral.small_int 1234)
        )
    ; MIdrop
    ];
  [%expect {|
    { PUSH (or bool int) (Right 1234) ; DROP }
    ->
    {} |}]

let%expect_test "lltz_specific_push_pair_literal" =
  test_instructions
    [ MIpush
        ( mt_pair mt_string mt_nat
        , MLiteral.pair (MLiteral.string "fst") (MLiteral.small_int 222) )
    ; MIpush (mt_string, MLiteral.string "rest code")
    ];
  [%expect {|
    { PUSH (pair string nat) (Pair "fst" 222) ; PUSH string "rest code" }
    ->
    { PUSH nat 222 ; PUSH string "fst" ; PAIR ; PUSH string "rest code" } |}]
let%expect_test "lltz_specific_dug_dig_swap_dropn" =
  test_instructions
    [ MIdig 1
    ; MIdropn 1
    ; MIdug 1
    ; MIdropn 1
    ; MIpush (mt_string, MLiteral.string "continuation")
    ];
  [%expect {|
    { DIG 1 ; DROP 1 ; DUG 1 ; DROP 1 ; PUSH string "continuation" }
    ->
    { DUG 2 ; DROP 2 ; PUSH string "continuation" } |}]

let%expect_test "lltz_specific_dug_dig_swap_dropn_more" =
  test_instructions
    [ MIswap
    ; MIdrop
    ; MIdug 1
    ; MIdropn 1
    ; MIpush (mt_nat, MLiteral.small_int 101)
    ];
  [%expect {|
    { SWAP ; DROP ; DUG 1 ; DROP 1 ; PUSH nat 101 }
    ->
    { DUG 2 ; DROP 2 ; PUSH nat 101 } |}]

let%expect_test "lltz_specific_dug_n_m_dropn_n_eq" =
  test_instructions
    [ MIdig 1
    ; MIdropn 1
    ; MIdug 2
    ; MIdropn 2
    ; MIpush (mt_string, MLiteral.string "done")
    ];
  [%expect {|
    { DIG 1 ; DROP 1 ; DUG 2 ; DROP 2 ; PUSH string "done" }
    ->
    { DUG 3 ; DROP 3 ; PUSH string "done" } |}]

let%expect_test "lltz_specific_mi2_dig_drop_midugn" =
  test_instructions
    [ MIdig 2
    ; MIdrop
    ; MIdug 3
    ; MIdug 3
    ; MIdropn 2
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect {|
    { DIG 2 ; DROP ; DUG 3 ; DUG 3 ; DROP 2 ; PUSH string "rest" }
    ->
    { DIG 2 ; DROP ; DIG 2 ; DROP ; DIG 2 ; DROP ; PUSH string "rest" } |}]

let%expect_test "lltz_specific_mi3_dig_drop_midugn_triple" =
  test_instructions
    [ MIdig 3
    ; MIdrop
    ; MIdug 4
    ; MIdug 4
    ; MIdug 4
    ; MIdropn 3
    ; MIpush (mt_int, MLiteral.small_int 999)
    ];
  [%expect {|
    { DIG 3 ; DROP ; DUG 4 ; DUG 4 ; DUG 4 ; DROP 3 ; PUSH int 999 }
    ->
    { DIG 3 ; DROP ; DIG 3 ; DROP ; DIG 3 ; DROP 2 ; PUSH int 999 } |}]

let%expect_test "lltz_specific_midig2_drop" =
  test_instructions
    [ MIdig 2
    ; MIdrop
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { DIG 2 ; DROP ; PUSH bool True }
    ->
    { DIG 2 ; DROP ; PUSH bool True } |}]

let%expect_test "lltz_specific_midig3_drop" =
  test_instructions
    [ MIdig 3
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "continuation")
    ];
  [%expect {|
    { DIG 3 ; DROP ; PUSH string "continuation" }
    ->
    { DIG 3 ; DROP ; PUSH string "continuation" } |}]

let%expect_test "lltz_specific_mipairn_unpair_alltrue" =
  test_instructions
    [ MIpairn 3
    ; MIunpair [true; true; true]
    ; MIpush (mt_nat, MLiteral.small_int 777)
    ];
  [%expect {|
    { PAIR 3 ; UNPAIR 3 ; PUSH nat 777 }
    ->
    { PUSH nat 777 } |}]

let%expect_test "lltz_specific_mdropn_1" =
  test_instructions
    [ MIdropn 1
    ; MIpush (mt_string, MLiteral.string "some code")
    ];
  [%expect {|
    { DROP 1 ; PUSH string "some code" }
    ->
    { DROP ; PUSH string "some code" } |}]

let%expect_test "lltz_specific_no_rewrite_scenario" =
  test_instructions
    [ MIpairn 2 
    ; MIunpair [true; true; true]
    ; MIdrop
    ];
  [%expect {|
    { PAIR 2 ; UNPAIR 3 ; DROP }
    ->
    { DROP ; UNPAIR } |}]

let%expect_test "lltz_specific_no_rewrite_edge_case" =
  test_instructions
    [ MIdig 2
    ; MIdropn 2
    ; MIpush (mt_unit, MLiteral.unit)
    ];
  [%expect {|
    { DIG 2 ; DROP 2 ; PUSH unit Unit }
    ->
    { DIG 2 ; DROP 2 ; UNIT } |}]

let rec mk_dig k n =
  if n = 0 then []
  else MIdig k :: mk_dig k (n - 1)

let rec mk_dug k n =
  if n = 0 then []
  else MIdug k :: mk_dug k (n - 1)

let%expect_test "digdug_cycles_dig_k_times" =
  test_instructions
    (mk_dig 3 3 @ [ MIpush (mt_string, MLiteral.string "rest") ]);
  [%expect {|
    { DIG 3 ; DIG 3 ; DIG 3 ; PUSH string "rest" }
    ->
    { DUG 3 ; PUSH string "rest" } |}]

let%expect_test "digdug_cycles_dug_k_times" =
  test_instructions
    (mk_dug 4 4 @ [ MIdrop ]);
  [%expect {|
    { DUG 4 ; DUG 4 ; DUG 4 ; DUG 4 ; DROP }
    ->
    { DIG 4 ; DROP } |}]

let%expect_test "digdug_cycles_dig1_or_dug1" =
  test_instructions
    [ MIdig 1
    ; MIdig 1
    ; MIdug 1
    ; MIdug 1
    ; MIpush (mt_nat, MLiteral.small_int 42)
    ];
  [%expect {|
    { DIG 1 ; DIG 1 ; DUG 1 ; DUG 1 ; PUSH nat 42 }
    ->
    { PUSH nat 42 } |}]

let%expect_test "digdug_cycles_incomplete_sequence" =
  test_instructions
    [ MIdig 2
    ; MIdig 2
    ; MIpush (mt_string, MLiteral.string "break sequence")
    ; MIdig 2
    ];
  [%expect {|
    { DIG 2 ; DIG 2 ; PUSH string "break sequence" ; DIG 2 }
    ->
    { DUG 2 ; PUSH string "break sequence" ; DIG 2 } |}]

let%expect_test "digdug_cycles_more_than_needed" =
  test_instructions
    (mk_dig 3 5 @ [ MIdrop ]);
  [%expect {|
    { DIG 3 ; DIG 3 ; DIG 3 ; DIG 3 ; DIG 3 ; DROP }
    ->
    { DIG 3 ; DROP } |}]

let%expect_test "digdug_cycles_interspersed_break" =
  test_instructions
    [ MIdig 3
    ; MIdig 3
    ; MIdrop 
    ; MIdig 3
    ; MIpush (mt_string, MLiteral.string "end")
    ];
  [%expect {|
    { DIG 3 ; DIG 3 ; DROP ; DIG 3 ; PUSH string "end" }
    ->
    { DIG 2 ; DROP ; DIG 2 ; DIG 3 ; PUSH string "end" } |}]

let%expect_test "digdug_cycles_dug_interspersed" =
  test_instructions
    [ MIdug 2
    ; MIdug 2
    ; MIswap 
    ; MIdug 2
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect {|
    { DUG 2 ; DUG 2 ; SWAP ; DUG 2 ; PUSH bool True }
    ->
    { DIG 2 ; SWAP ; DUG 2 ; PUSH bool True } |}]

let%expect_test "unpair_empty" =
  test_instructions
    [ MIunpair []
    ; MIpush (mt_nat, MLiteral.small_int 42)
    ];
  [%expect {|
    { UNPAIR 0 ; PUSH nat 42 }
    ->
    { PUSH nat 42 } |}]

let%expect_test "unpair_single_true" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdrop
    ];
  [%expect{|
    { UNPAIR ; DROP }
    ->
    { CDR } |}]

let%expect_test "unpair_single_false" =
  test_instructions
    [ MIunpair [true]
    ; MIpush (mt_string, MLiteral.string "rest")
    ];
  [%expect{|
    { UNPAIR 1 ; PUSH string "rest" }
    ->
    { PUSH string "rest" } |}]

let%expect_test "unpair_true_true_drop" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdrop
    ; MIdig 1
    ];
  [%expect {|
    { UNPAIR ; DROP ; DIG 1 }
    ->
    { CDR ; SWAP } |}]

let%expect_test "unpair_true_false_drop" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdrop
    ; MIpush (mt_bool, MLiteral.bool true)
    ];
  [%expect{|
    { UNPAIR ; DROP ; PUSH bool True }
    ->
    { CDR ; PUSH bool True } |}]

let%expect_test "unpair_complex_fields_first_is_true" =
  test_instructions
    [ MIunpair [true; true; true]
    ; MIdrop
    ; MIpush (mt_unit, MLiteral.unit)
    ];
  [%expect {|
    { UNPAIR 3 ; DROP ; PUSH unit Unit }
    ->
    { CDR ; UNPAIR ; UNIT } |}]

let%expect_test "unpair_complex_fields_first_is_false" =
  test_instructions
    [ MIunpair [true; true; true]
    ; MIpush (mt_string, MLiteral.string "rest code")
    ];
  [%expect{|
    { UNPAIR 3 ; PUSH string "rest code" }
    ->
    { UNPAIR 3 ; PUSH string "rest code" } |}]

let%expect_test "unpair_drop_field_beyond_arities" =
  test_instructions
    [ MIunpair [true; true; true]
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_bool, MLiteral.bool false)
    ];
  [%expect {|
    { UNPAIR 3 ; DIG 2 ; DROP ; PUSH bool False }
    ->
    { UNPAIR 3 ; DIG 2 ; DROP ; PUSH bool False } |}]

let%expect_test "unpair_full_drop_when_n_ge_k" =
  test_instructions
    [ MIunpair [true; true; true]
    ; MIdig 3
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 111)
    ];
  [%expect {|
    { UNPAIR 3 ; DIG 3 ; DROP ; PUSH int 111 }
    ->
    { SWAP ; DROP ; UNPAIR 3 ; PUSH int 111 } |}]

let%expect_test "unpair_true_true_dig1_drop" =
  test_instructions
    [ MIunpair [true; true]
    ; MIdig 1
    ; MIdrop
    ; MIpush (mt_string, MLiteral.string "extra")
    ];
  [%expect {|
    { UNPAIR ; DIG 1 ; DROP ; PUSH string "extra" }
    ->
    { CAR ; PUSH string "extra" } |}]

let%expect_test "unpair_no_rewrite_nonmatching_pattern" =
  test_instructions
    [ MIunpair [true; true]
    ; MI2 Add
    ; MIpush (mt_unit, MLiteral.unit)
    ];
  [%expect {|
    { UNPAIR ; ADD ; PUSH unit Unit }
    ->
    { UNPAIR ; ADD ; UNIT } |}]

let%expect_test "composite_test_1_complex_push_drop_fail" =
  test_instructions
    [ 
      MIcomment ["comment A"]
    ; MIcomment ["comment B"]
    ; MIseq
        [ { instr = MIpush (mt_int, MLiteral.small_int 123) }
        ; { instr = MIdrop }
        ; { instr = MIpush (mt_int, MLiteral.small_int 123) }
        ]
    ; MIdup 1
    ; MIdig 1
    ; MIswap
    ; MIpush (mt_string, MLiteral.string "some error")
    ; MI1_fail Failwith
    ; MIpush (mt_nat, MLiteral.small_int 42)
    ; MIpush (mt_nat, MLiteral.small_int 42)
    ; MI1 Not
    ];
  [%expect {|
    { PUSH int 123 ;
      DROP ;
      PUSH int 123 ;
      DUP ;
      DIG 1 ;
      SWAP ;
      PUSH string "some error" ;
      FAILWITH ;
      PUSH nat 42 ;
      PUSH nat 42 ;
      NOT }
    ->
    { PUSH string "some error" ; FAILWITH } |}]

  let%expect_test "composite_test_2_macro_expansion_unpair_bubble" =
  test_instructions
    [ 
      MIsetField [ A; D ]
    ; MIunpair [true; true; true]
    ; MIdip { instr = MIseq [] }
    ; MIdig 1
    ; MIdig 1
    ; MIpush (mt_string, MLiteral.string "some-literal")
    ; MIdig 1
    ; MIdrop
    ; MIfield [ D ]
    ; MIdrop
    ];
  [%expect{|
    { DUP ;
      DIP { CAR ; CAR ; PAIR } ;
      CDR ;
      SWAP ;
      PAIR ;
      UNPAIR 3 ;
      DIP {} ;
      DIG 1 ;
      DIG 1 ;
      PUSH string "some-literal" ;
      DIG 1 ;
      DROP ;
      CDR ;
      DROP }
    ->
    { DUP ; DIP { CAR ; CAR ; PAIR } ; SWAP ; DROP ; CDR ; UNPAIR } |}]

  let%expect_test "composite_test_3_lists_and_iters" =
  test_instructions
    [ 
      MI0 (Nil mt_int)
    ; MIpush (mt_int, MLiteral.small_int 999)
    ; MI2 Cons
    ; 
      MIif_cons
        ( { instr =
              MIseq
                [ { instr = MIdig 2 }
                ; { instr = MIdrop }
                ; { instr = MIpush (mt_string, MLiteral.string "cons-branch") }
                ]
          }
        , { instr =
              MIseq
                [ { instr = MIpush (mt_string, MLiteral.string "fail-branch") }
                ; { instr = MI1_fail Failwith }
                ]
          } )
    ; MIpush (mt_bool, MLiteral.bool false)
    ; MIloop { instr = MI1 Neg }
    ; MIdip { instr = MIdrop }
    ; MIdig 1
    ; MIdrop
    ];
  [%expect {|
    { NIL int ;
      PUSH int 999 ;
      CONS ;
      IF_CONS
        { DIG 2 ; DROP ; PUSH string "cons-branch" }
        { PUSH string "fail-branch" ; FAILWITH } ;
      PUSH bool False ;
      LOOP { NEG } ;
      DIP { DROP } ;
      DIG 1 ;
      DROP }
    ->
    { DROP ;
      NIL int ;
      PUSH int 999 ;
      CONS ;
      IF_CONS { DROP 2 } { PUSH string "fail-branch" ; FAILWITH } ;
      PUSH string "cons-branch" } |}]

  let%expect_test "composite_test_4_or_and_pair_interactions" =
  test_instructions
    [ 
      MIpush
        ( mt_or mt_string mt_int
        , MLiteral.right (MLiteral.small_int 10)
        )
    ; MIdig 1
    ; MIdrop
    ; MIpush
        ( mt_pair mt_bool mt_nat
        , MLiteral.pair (MLiteral.bool true) (MLiteral.small_int 42)
        )
    ; MIfield [ A ]
    ; MIpush (mt_bool, MLiteral.bool true)
    ; MIpush (mt_bool, MLiteral.bool false)
    ; MI2 And
    ; MI1 (Right (None, None, mt_string))
    ; MIloop_left { instr = MIswap }
    ; MIpush (mt_int, MLiteral.small_int 1234)
    ; MIdrop
    ];
  [%expect {|
    { PUSH (or string int) (Right 10) ;
      DIG 1 ;
      DROP ;
      PUSH (pair bool nat) (Pair True 42) ;
      CAR ;
      PUSH bool True ;
      PUSH bool False ;
      AND ;
      RIGHT string ;
      LOOP_LEFT { SWAP } ;
      PUSH int 1234 ;
      DROP }
    ->
    { DROP ; PUSH int 10 ; RIGHT string ; PUSH bool True ; PUSH bool False } |}]

  let%expect_test "composite_test_5_digdug_chaos" =
  test_instructions
    [
      MIdig 3
    ; MIdig 3
    ; MIpush (mt_string, MLiteral.string "break pattern")
    ; MIdig 3
    ; MIdug 3
    ; MIdig 3
    ; MI2 Compare
    ; MIdrop
    ; MIdrop
    ; MIdup 1
    ; MIdig 2
    ; MIdrop
    ; MIdig 2
    ; MIdig 2
    ; MIdig 2
    ; MIpush (mt_nat, MLiteral.small_int 777)
    ];
  [%expect {|
    { DIG 3 ;
      DIG 3 ;
      PUSH string "break pattern" ;
      DIG 3 ;
      DUG 3 ;
      DIG 3 ;
      COMPARE ;
      DROP ;
      DROP ;
      DUP ;
      DIG 2 ;
      DROP ;
      DIG 2 ;
      DIG 2 ;
      DIG 2 ;
      PUSH nat 777 }
    ->
    { DROP 3 ; DUP ; PUSH nat 777 } |}]

  let%expect_test "composite_test_6_big_map_update_folding" =
  test_instructions
    [ 
      MIpush
        ( mt_map mt_string mt_bool
        , MLiteral.mk_map []
        )
    ; MIpush (mt_bool, MLiteral.some (MLiteral.bool true))
    ; MIpush (mt_string, MLiteral.string "hello-key")
    ; MI3 Update
    ; MIpush (mt_bool, MLiteral.some (MLiteral.bool false))
    ; MIpush (mt_string, MLiteral.string "other-key")
    ; MI3 Update
    ; MIdup 1
    ; MIdrop
    ; MIpush (mt_int, MLiteral.small_int 2)
    ; MIpush (mt_int, MLiteral.small_int 3)
    ; MI2 Add
    ; MIpush (mt_bool, MLiteral.bool true)
    ; MIdrop
    ];
  [%expect {|
    { PUSH (map string bool) {} ;
      PUSH bool (Some True) ;
      PUSH string "hello-key" ;
      UPDATE ;
      PUSH bool (Some False) ;
      PUSH string "other-key" ;
      UPDATE ;
      DUP ;
      DROP ;
      PUSH int 2 ;
      PUSH int 3 ;
      ADD ;
      PUSH bool True ;
      DROP }
    ->
    { PUSH (map string bool) {} ;
      PUSH bool (Some True) ;
      PUSH string "hello-key" ;
      UPDATE ;
      PUSH bool (Some False) ;
      PUSH string "other-key" ;
      UPDATE ;
      PUSH int 5 } |}]

  let%expect_test "composite_test_7_create_contract_shortcut" =
  test_instructions
    [ 
      MI2 (Pair (None, None))
    ; MIdup 1
    ; MIfield [ A ]
    ; MI0 (Nil mt_operation)
    ; MIdig 1
    ; MI2 Cons
    ; MIdig 1
    ; MIfield [ D ]
    ; MIdig 2
    ; MIdrop
    ; MIpush (mt_bool, MLiteral.bool false)
    ; MIloop { instr = MIswap }
    ];
  [%expect {|
    { PAIR ;
      DUP ;
      CAR ;
      NIL operation ;
      DIG 1 ;
      CONS ;
      DIG 1 ;
      CDR ;
      DIG 2 ;
      DROP ;
      PUSH bool False ;
      LOOP { SWAP } }
    ->
    { DIG 2 ; DROP ; NIL operation ; SWAP ; CONS ; SWAP } |}]

  let%expect_test "composite_test_8_unpair_selective_fields_dig_drop" =
  test_instructions
    [ 
      MIunpair [true; true; true]
    ; MIdig 2
    ; MIdrop
    ; MIdip { instr = MIseq [ mk MIdrop ] }
    ; MIpush (mt_int, MLiteral.small_int 0)
    ; MI1 Neg
    ; MIpush (mt_string, MLiteral.string "end")
    ];
  [%expect{|
    { UNPAIR 3 ;
      DIG 2 ;
      DROP ;
      DIP { DROP } ;
      PUSH int 0 ;
      NEG ;
      PUSH string "end" }
    ->
    { UNPAIR 3 ; DUG 2 ; DROP 2 ; PUSH int 0 ; PUSH string "end" } |}]

  let%expect_test "composite_test_9_lambda_inlining_plus_arithmetic" =
  test_instructions
    [ 
      MIlambda
        ( mt_nat
        , mt_nat
        , { instr =
              MIseq
                [ mk (MIpush (mt_int, MLiteral.small_int 2))
                ; mk (MI2 Mul)
                ; mk (MI1 Neg)
                ]
          } )
    ; MIdig 1
    ; MI2 Exec
    ; MIpush (mt_bool, MLiteral.bool false)
    ; MIif
        ( { instr = MIpush (mt_string, MLiteral.string "true branch") }
        , { instr = MIpush (mt_string, MLiteral.string "false branch") } )
    ; MIpush (mt_nat, MLiteral.small_int 100)
    ; MIdrop
    ];
  [%expect {|
    { LAMBDA nat nat { PUSH int 2 ; MUL ; NEG } ;
      DIG 1 ;
      EXEC ;
      PUSH bool False ;
      IF { PUSH string "true branch" } { PUSH string "false branch" } ;
      PUSH nat 100 ;
      DROP }
    ->
    { PUSH int 2 ; MUL ; NEG ; PUSH string "false branch" } |}]

  let%expect_test "composite_test_10_lltz_specific_clashes" =
  test_instructions
    [ 
      MIpush
        ( mt_list mt_string
        , MLiteral.list [MLiteral.string "one-element"] )
    ; MIdrop
    ; MIdig 2
    ; MIdrop
    ; MIdig 2
    ; MIdrop
    ; MIdug 3
    ; MIdug 3
    ; MIdropn 2
    ; MIpush (mt_bool, MLiteral.bool true)
    ; MIdrop
    ];
  [%expect {|
    { PUSH (list string) { "one-element" } ;
      DROP ;
      DIG 2 ;
      DROP ;
      DIG 2 ;
      DROP ;
      DUG 3 ;
      DUG 3 ;
      DROP 2 ;
      PUSH bool True ;
      DROP }
    ->
    { DIG 2 ; DROP ; DIG 2 ; DROP ; DIG 2 ; DROP ; DIG 2 ; DROP } |}]
