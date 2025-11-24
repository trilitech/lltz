(*
   lltz_michelson.ml
   Compiles types, constants, primitives and expressions from LLTZ-IR to Michelson Ast.
*)

module Stack = Stack
module Type = Type
module Instruction = Instruction
module Slot = Slot
module Last_vars = Last_vars
open Core

module LLTZ = struct
  module E = Lltz_ir.Expr
  module T = Lltz_ir.Type
  module R = Lltz_ir.Row
  module P = Lltz_ir.Primitive
  module Dsl = Lltz_ir.Ast_builder.Default
  module Dsl_with_dummy = Lltz_ir.Ast_builder.With_dummy
  module Free_vars = Lltz_ir.Free_vars
end

module Michelson = struct
  module Ast = Lltz_michelson.Ast
  module T = Lltz_michelson.Ast.Type
end

open Instruction
open Tezos_micheline

(* If label does not begin with % add it to the beginning*)
let sanitize_label annot =
  match annot with
  | Some label ->
    if String.is_prefix label ~prefix:"%" then Some label else Some ("%" ^ label)
  | None -> None
;;

let rec compile_row_types ?(annot = None) row =
  match row with
  | LLTZ.R.Node nodes ->
    Type.tuple
      ~annot:(sanitize_label annot)
      (List.map nodes ~f:(compile_row_types ~annot:None))
  | LLTZ.R.Leaf (Some (LLTZ.R.Label annot), value) ->
    convert_type ~annot:(sanitize_label (Some annot)) value
  | LLTZ.R.Leaf (None, value) -> convert_type value

and compile_row_types_for_or ?(annot = None) row =
  match row with
  | LLTZ.R.Node nodes ->
    let converted_types = List.map nodes ~f:(compile_row_types_for_or ~annot:None) in
    Type.ors ~annot:(sanitize_label annot) converted_types
  | LLTZ.R.Leaf (Some (LLTZ.R.Label annot), value) ->
    convert_type ~annot:(sanitize_label (Some annot)) value
  | LLTZ.R.Leaf (None, value) -> convert_type value

and convert_type ?(annot = None) (ty : LLTZ.T.t) : Michelson.Ast.t =
  match ty.desc with
  | Tuple row -> compile_row_types ~annot row
  | Or row -> compile_row_types_for_or ~annot row
  | Option ty -> Michelson.T.option ~annot (convert_type ty)
  | List ty -> Michelson.T.list ~annot (convert_type ty)
  | Set ty -> Michelson.T.set ~annot (convert_type ty)
  | Contract ty -> Michelson.T.contract ~annot (convert_type ty)
  | Ticket ty -> Michelson.T.ticket ~annot (convert_type ty)
  | Function (param, ret) ->
    Michelson.T.lambda ~annot (convert_type param) (convert_type ret)
  | Map (key, value) -> Michelson.T.map ~annot (convert_type key) (convert_type value)
  | Big_map (key, value) ->
    Michelson.T.big_map ~annot (convert_type key) (convert_type value)
  | Unit -> Michelson.T.unit ~annot ()
  | Bool -> Michelson.T.bool ~annot ()
  | Nat -> Michelson.T.nat ~annot ()
  | Int -> Michelson.T.int ~annot ()
  | Mutez -> Michelson.T.mutez ~annot ()
  | String -> Michelson.T.string ~annot ()
  | Bytes -> Michelson.T.bytes ~annot ()
  | Chain_id -> Michelson.T.chain_id ~annot ()
  | Timestamp -> Michelson.T.timestamp ~annot ()
  | Address -> Michelson.T.address ~annot ()
  | Keys -> Michelson.T.key ~annot ()
  | Key_hash -> Michelson.T.key_hash ~annot ()
  | Signature -> Michelson.T.signature ~annot ()
  | Operation -> Michelson.T.operation ~annot ()
  | Sapling_state { memo } -> Michelson.T.sampling_state ~annot (Michelson.Ast.int memo)
  | Sapling_transaction { memo } ->
    Michelson.T.sapling_transaction ~annot (Michelson.Ast.int memo)
  | Never -> Michelson.T.never ~annot ()
  | Bls12_381_g1 -> Michelson.T.bls12_381_g1 ~annot ()
  | Bls12_381_g2 -> Michelson.T.bls12_381_g2 ~annot ()
  | Bls12_381_fr -> Michelson.T.bls12_381_fr ~annot ()
  | Chest_key -> Michelson.T.chest_key ~annot ()
  | Chest -> Michelson.T.chest ~annot ()
  | Tx_rollup_l2_address -> Michelson.T.tx_rollup_l2_address ~annot ()

and int_of_timestamp_str s =
  match Ptime.of_rfc3339 s with
  | Ok (ptime, _, _) ->
    let span = Ptime.to_span ptime in
    (match Ptime.Span.to_int_s span with
     | Some secs -> secs
     | None -> assert false)
  | Error _ -> assert false

and convert_constant (const : LLTZ.E.constant) : Michelson.Ast.t =
  match const with
  | Unit -> Michelson.Ast.Instruction.unit
  | Bool b -> if b then Michelson.Ast.true_ else Michelson.Ast.false_
  | Nat n -> Michelson.Ast.int (Z.to_int n)
  | Int n -> Michelson.Ast.int (Z.to_int n)
  | Mutez n -> Michelson.Ast.int (Z.to_int n)
  | String s -> Michelson.Ast.string s
  | Key s -> Michelson.Ast.string s
  | Key_hash s -> Michelson.Ast.string s
  | Bytes s -> Michelson.Ast.(bytes (Bytes.of_string s))
  | Chain_id s -> Michelson.Ast.string s
  | Address s -> Michelson.Ast.string s
  | Timestamp s -> Michelson.Ast.int (int_of_timestamp_str s)
  | Bls12_381_g1 s -> Michelson.Ast.(bytes (Bytes.of_string s))
  | Bls12_381_g2 s -> Michelson.Ast.(bytes (Bytes.of_string s))
  | Bls12_381_fr s -> Michelson.Ast.(bytes (Bytes.of_string s))
  | Signature s -> Michelson.Ast.string s
;;

let get_const_type (const : LLTZ.E.constant) : Michelson.Ast.t =
  match const with
  | Unit -> Michelson.T.unit ()
  | Bool _ -> Michelson.T.bool ()
  | Nat _ -> Michelson.T.nat ()
  | Int _ -> Michelson.T.int ()
  | Mutez _ -> Michelson.T.mutez ()
  | String _ -> Michelson.T.string ()
  | Key _ -> Michelson.T.key ()
  | Key_hash _ -> Michelson.T.key_hash ()
  | Bytes _ -> Michelson.T.bytes ()
  | Chain_id _ -> Michelson.T.chain_id ()
  | Address _ -> Michelson.T.address ()
  | Timestamp _ -> Michelson.T.timestamp ()
  | Bls12_381_g1 _ -> Michelson.T.bls12_381_g1 ()
  | Bls12_381_g2 _ -> Michelson.T.bls12_381_g2 ()
  | Bls12_381_fr _ -> Michelson.T.bls12_381_fr ()
  | Signature _ -> Michelson.T.signature ()
;;

let convert_primitive (prim : LLTZ.P.t) : Michelson.Ast.t =
  let open Michelson.Ast.Instruction in
  match prim with
  | Amount -> amount
  | Balance -> balance
  | Chain_id -> chain_id
  | Level -> level
  | Now -> now
  | Self opt -> self opt
  | Self_address -> self_address
  | Sender -> sender
  | Source -> source
  | Total_voting_power -> total_voting_power
  | Empty_bigmap (ty1, ty2) -> empty_big_map (convert_type ty1) (convert_type ty2)
  | Empty_map (ty1, ty2) -> empty_map (convert_type ty1) (convert_type ty2)
  | Empty_set cty -> empty_set (convert_type cty)
  | Nil ty -> nil (convert_type ty)
  | None ty -> none (convert_type ty)
  | Sapling_empty_state { memo } -> sapling_empty_state memo
  | Unit -> unit
  | Car -> car
  | Cdr -> cdr
  | Left (_, _, ty) -> left (convert_type ty)
  | Right (_, _, ty) -> right (convert_type ty)
  | Some -> some
  | Eq -> eq
  | Abs -> abs
  | Neg -> neg
  | Nat -> nat
  | Int -> int
  | Bytes -> bytes
  | Is_nat -> is_nat
  | Neq -> neq
  | Le -> le
  | Lt -> lt
  | Ge -> ge
  | Gt -> gt
  | Not -> not
  | Size -> size
  | Address -> address
  | Implicit_account -> implicit_account
  | Is_implicit_account -> is_implicit_account
  | Contract (annot, ty) -> contract ~annot (convert_type ty)
  | Pack -> pack
  | Unpack ty -> unpack (convert_type ty)
  | Hash_key -> hash_key
  | Blake2b -> blake2b
  | Sha256 -> sha256
  | Sha512 -> sha512
  | Keccak -> keccak
  | Sha3 -> sha3
  | Set_delegate -> set_delegate
  | Read_ticket -> Michelson.Ast.seq [ read_ticket; pair () ]
  | Join_tickets -> join_tickets
  | Pairing_check -> pairing_check
  | Voting_power -> voting_power
  | Get_n n -> get_n n
  | Cast ty -> cast (convert_type ty)
  | Rename _ -> failwith (* Instruction does not exist. *)
  | Emit (opt, ty_opt) -> emit opt (Option.map ~f:convert_type ty_opt)
  | Failwith -> assert false (* Resolved in compile_prim *)
  | Never -> assert false (* Resolved in compile_prim *)
  | Pair (opt1, opt2) -> pair ~left_annot:opt1 ~right_annot:opt2 ()
  | Add -> add
  | Mul -> mul
  | Sub -> sub
  | Sub_mutez -> sub_mutez
  | Lsr -> lsr_
  | Lsl -> lsl_
  | Xor -> xor
  | Ediv -> ediv
  | And -> and_
  | Or -> or_
  | Cons -> cons
  | Compare -> compare
  | Concat1 -> concat1
  | Concat2 -> concat2
  | Get -> get
  | Mem -> mem
  | Exec -> exec
  | Apply -> apply
  | Sapling_verify_update -> sapling_verify_update
  | Ticket -> ticket
  | Ticket_deprecated -> ticket_deprecated
  | Split_ticket -> split_ticket
  | Update_n n -> update_n n
  | View (name, ty) -> view name (convert_type ty)
  | Slice -> slice
  | Update -> update
  | Get_and_update -> Michelson.Ast.seq [ get_and_update; pair () ]
  | Transfer_tokens -> transfer_tokens
  | Check_signature -> check_signature
  | Open_chest -> open_chest
;;

let rec compile : LLTZ.E.t -> t =
  fun expr ->
  seq
    [ (match expr.desc with
       | Variable (Var name) -> compile_variable name expr.type_ expr.annotations
       | Let_in { let_var = Var _; _ } -> compile_let_in expr
       | Lambda _ -> compile_lambda expr
       | Lambda_rec _ -> compile_lambda_rec expr
       | App { abs; arg } -> compile_app abs arg
       | Const constant -> compile_const constant
       | Prim (primitive, args) -> compile_prim primitive args
       | Let_mut_in _ -> compile_mut_let_in expr
       | Deref (Mut_var var) -> compile_deref var expr.type_ expr.annotations
       | Assign (Mut_var _, _) -> compile_assign expr
       | If_bool { condition; if_true; if_false } ->
         compile_if_bool condition if_true if_false
       | If_none { subject; if_none; if_some = { lam_var = Var var, _; body = some } } ->
         compile_if_none subject if_none (var, some)
       | If_cons
           { subject
           ; if_empty
           ; if_nonempty = { lam_var1 = Var hd, _; lam_var2 = Var tl, _; body = nonempty }
           } -> compile_if_cons subject if_empty (hd, tl, nonempty)
       | If_left
           { subject
           ; if_left = { lam_var = Var left, _; body = l }
           ; if_right = { lam_var = Var right, _; body = r }
           } -> compile_if_left subject (left, l) (right, r)
       | While { cond; body } -> compile_while cond body
       | While_left { cond; body = { lam_var = Var var, _; body = body_lambda } } ->
         compile_while_left cond var body_lambda expr.type_
       | For { index = Mut_var var; init; cond; update; body } ->
         compile_for var init cond update body
       | For_each _ ->
         compile_for_each expr
       | Map { collection; map = { lam_var = Var var, _; body = lam_body } } ->
         compile_map collection var lam_body
       | Fold_left
           { collection
           ; init = init_body
           ; fold = { lam_var = Var var, _; body = fold_body }
           } -> compile_fold_left collection init_body var fold_body
       | Fold_right
           { collection
           ; init = init_body
           ; fold = { lam_var = Var var, _; body = fold_body }
           } -> compile_fold_right collection init_body var fold_body
       | Let_tuple_in _ -> compile_let_tuple_in expr
       | Tuple row -> compile_tuple row
       | Proj (tuple, path) -> compile_proj tuple path
       | Update { tuple; component; update } -> compile_update tuple component update
       | Inj (path, expr) -> compile_inj path expr
       | Match (subject, cases) -> compile_match subject cases
       | Raw_michelson { michelson; args } -> compile_raw_michelson michelson args
       | Create_contract
           { storage
           ; code = { lam_var = Var binder_var, binder_ty; body = code_body }
           ; delegate
           ; initial_balance
           ; initial_storage
           } ->
         compile_create_contract
           storage
           binder_var
           binder_ty
           code_body
           delegate
           initial_balance
           initial_storage
       | Global_constant { hash; args } -> compile_global_constant hash args expr.type_)
    ]

and unused_set (e : LLTZ.E.t) = e.annotations.remove_never_used_vars

and compile_contract (input_var : string) (code : LLTZ.E.t) =
  let code_instr = compile code in
  let unused_set =
    if Set.mem code.annotations.last_used_vars input_var
    then unused_set code
    else Set.add (unused_set code) input_var
  in
  seq [ Slot.mock_value; Slot.let_ (`Ident input_var) ~unused_set ~in_:code_instr ]

(* Compile a variable by duplicating its value on the stack. *)
and compile_variable name type_ annotations =
  trace
    ~flag:(String.append "var " name)
    (Slot.dup_or_dig
       (`Ident name)
       (LLTZ.T.is_duppable type_ && Stdlib.not (Set.mem annotations.last_used_vars name)))

(* Compile a let-in expression by compiling the right-hand side, then binding the result to the variable in the inner expression. *)
and compile_let_in expr =
  match expr.desc with
  | Let_in { let_var = Var var; rhs; in_ } ->
    if Set.mem expr.annotations.remove_never_used_vars var
    then
      trace ~flag:(String.append "let in " var) (seq [ compile rhs; drop 1; compile in_ ])
    else
      trace
        ~flag:(String.append "let in " var)
        (seq
           [ compile rhs
           ; Slot.let_ (`Ident var) ~unused_set:(unused_set expr) ~in_:(compile in_)
           ])
  | _ -> assert false

(* Compile a mutable let-in expression by compiling the right-hand side, then binding the result to the mutable variable in the inner expression. *)
and compile_mut_let_in expr =
  match expr.desc with
  | Let_mut_in { let_var = Mut_var var; rhs; in_ } ->
    if Set.mem expr.annotations.remove_never_used_vars var
    then
      trace
        ~flag:(String.append "let_mut in " var)
        (seq [ compile rhs; drop 1; compile in_ ])
    else
      trace
        ~flag:(String.append "let_mut in " var)
        (seq
           [ compile rhs
           ; Slot.let_ (`Ident var) ~unused_set:(unused_set expr) ~in_:(compile in_)
           ])
  | _ -> assert false

(* Compile a constant by pushing its value onto the stack. *)
and compile_const constant =
  match constant with
  | LLTZ.E.Unit -> seq [ unit ]
  | _ -> seq [ push (get_const_type constant) (convert_constant constant) ]

(* Compile a primitive by compiling its arguments, then applying the primitive to the arguments. *)
and compile_prim primitive args =
  trace
    ~flag:(Sexp.to_string_hum (LLTZ.P.sexp_of_t primitive))
    (let args_instrs = List.map ~f:compile args in
     match primitive with
     | LLTZ.P.Failwith -> seq (List.rev_append args_instrs [ Instruction.failwith ])
     | LLTZ.P.Never -> seq (List.rev_append args_instrs [ Instruction.never ])
     | LLTZ.P.Sub ->
       seq
         (List.rev args_instrs
          @ [ prim
                ~message:(Sexp.to_string_hum (LLTZ.P.sexp_of_t primitive))
                (List.length args)
                1
                (convert_primitive primitive)
            ])
     | _ ->
       seq
         (List.rev args_instrs
          @ [ prim
                ~message:(Sexp.to_string_hum (LLTZ.P.sexp_of_t primitive))
                (List.length args)
                1
                (convert_primitive primitive)
            ]))

(* Compile a dereference by duplicating the value of the mutable variable on the stack. *)
and compile_deref name type_ annotations =
  trace
    ~flag:(String.append "mut_var " name)
    (Slot.dup_or_dig
       (`Ident name)
       (LLTZ.T.is_duppable type_ && Stdlib.not (Set.mem annotations.last_used_vars name)))

(* Compile an assignment by compiling the value to be assigned, then assigning it to the slot corresponding to the mutable variable. *)
and compile_assign expr =
  (*TODO merge with last used opt, do also for lets*)
  match expr.desc with
  | Assign (Mut_var var, value) ->
    if Set.mem expr.annotations.last_used_vars var
       || Set.mem expr.annotations.remove_never_used_vars var
    then
      trace
        ~flag:"assign"
        (seq [ compile value; Slot.collect (`Ident var); drop 1; unit ])
    else trace ~flag:"assign" (seq [ trace (compile value); Slot.set (`Ident var); unit ])
  | _ -> assert false

and remove_unused (e : LLTZ.E.t) =
  let unused = e.annotations.remove_never_used_vars in
  let unused_list = Set.elements unused in
  Slot.collect_all (List.map ~f:(fun x -> `Ident x) unused_list)

and remove_last_used (e : LLTZ.E.t) =
  let last_used = e.annotations.last_used_vars in
  let last_used_list = Set.elements last_used in
  Slot.collect_all (List.map ~f:(fun x -> `Ident x) last_used_list)

(* Compile an if-bool expression by compiling the condition, then applying the if-bool instruction to the condition and the true and false branches. *)
and compile_if_bool condition if_true if_false =
  seq
    [ compile condition
    ; if_
        ~then_:(seq [ remove_unused if_true; compile if_true ])
        ~else_:(seq [ remove_unused if_false; compile if_false ])
    ]

(* Compile an if-none expression by compiling the subject, then applying the if-none instruction to the subject and the none and some branches. *)
and compile_if_none subject if_none_clause (var, some_clause) =
  seq
    [ compile subject
    ; if_none
        ~none:(seq [ remove_unused if_none_clause; compile if_none_clause ])
        ~some:
          (Slot.let_
             (`Ident var)
             ~unused_set:(unused_set some_clause)
             ~in_:(seq [ remove_unused some_clause; compile some_clause ]))
    ]

(* Compile an if-cons expression by compiling the subject, then applying the if-cons instruction to the subject and the empty and nonempty branches. *)
and compile_if_cons subject if_empty (hd, tl, nonempty) =
  trace
    (seq
       [ compile subject
       ; if_cons
           ~empty:(seq [ remove_unused if_empty; compile if_empty ])
           ~nonempty:
             (Slot.let_all
                [ `Ident hd; `Ident tl ]
                ~unused_set:(unused_set nonempty)
                ~in_:(seq [ remove_unused nonempty; compile nonempty ]))
       ])

(* Compile an if-left expression by compiling the subject, then applying the if-left instruction to the subject and the left and right branches. *)
and compile_if_left subject (left, l) (right, r) =
  seq
    [ compile subject
    ; if_left
        ~left:
          (Slot.let_
             (`Ident left)
             ~unused_set:(unused_set l)
             ~in_:(seq [ remove_unused l; compile l ]))
        ~right:
          (Slot.let_
             (`Ident right)
             ~unused_set:(unused_set r)
             ~in_:(seq [ remove_unused r; compile r ]))
    ]

(* Compile a while expression by compiling the invariant, then applying the loop instruction to the body and invariant. *)
and compile_while invariant body =
  seq [ compile invariant; loop (seq [ compile body; drop 1; compile invariant ]); unit ]

(* Compile a while-left expression by compiling the invariant, then applying the loop-left instruction to the body and invariant. *)
and compile_while_left init_val var body_lambda res_ty =
  seq
    [ compile init_val
    ; left (convert_type res_ty)
    ; loop_left
        (seq
           [ Slot.let_
               (`Ident var)
               ~unused_set:(unused_set body_lambda)
               ~in_:(compile body_lambda)
           ])
    ]

(* Compile a for expression by compiling the initial value, invariant, variant, and body,
   then applying the loop to the sequence of body, variant, and invariant. *)
and compile_for index init invariant variant body =
  let init_instr = compile init in
  let inv_instr = compile invariant in
  seq
    [ init_instr
    ; Slot.let_
        (`Ident index)
        ~in_:
          (seq
             [ inv_instr
             ; loop (seq [ compile body; drop 1; compile variant; drop 1; inv_instr ])
             ])
    ; unit
    ]

(* Compile a tuple expression by compiling each component and pairing them together. *)
and compile_tuple row =
  trace
    ~flag:"tuple"
    (match row with
     | LLTZ.R.Node nodes ->
       let compiled_nodes = List.map ~f:compile_tuple nodes in
       seq ((List.rev_append compiled_nodes) [ pair_n (List.length compiled_nodes) ])
     | LLTZ.R.Leaf (_, value) -> compile value)

(* Compile a projection expression by compiling the tuple and then getting the nth element. *)
and compile_proj tuple path =
  let _, _, tuple_expanded_instr = expand_tuple tuple path false in
  trace ~flag:"proj" (seq [ trace ~flag:"proj expansion" tuple_expanded_instr ])

(* Compile an update expression by compiling the tuple row, getting the nth element, compiling the update value, and combining the values back together into tuple. *)
and compile_update tuple component update =
  let lengths, gets, _ = expand_tuple tuple component true in
  let updates =
    List.rev
      (match component with
       | LLTZ.R.Path.Here list ->
         List.mapi list ~f:(fun i num ->
           match List.nth lengths i with
           | Some length -> update_n num ~length
           | None ->
             raise_s
               [%message
                 "compile_update: index out of bounds in updates"
                   (i : int)
                   (lengths : int list)]))
  in
  trace
    ~flag:"update"
    (seq
       ([ compile tuple ]
        @ [ trace ~flag:"gets" (seq gets) ]
        @ [ drop 1; compile update ]
        @ [ trace ~flag:"updates" (seq updates) ]))

and get_lengths_inner row path_list =
  match row with
  | LLTZ.R.Node nodes ->
    (match path_list with
     | hd :: tl ->
       (match List.nth nodes hd with
        | Some node -> List.length nodes :: get_lengths_inner node tl
        | None ->
          raise_s
            [%message
              "get_lengths: index out of bounds"
                (hd : int)
                (nodes : LLTZ.T.t LLTZ.R.t list)])
     | [] -> [])
  | LLTZ.R.Leaf (_, _) -> [ 1 ]

(* Get the number of children for each node on the path *)
and get_tuple_lengths tuple path =
  match LLTZ.E.(tuple.type_), path with
  | { desc = LLTZ.T.Tuple row; _ }, LLTZ.R.Path.Here list -> get_lengths_inner row list
  | _ -> raise_s [%message "Tuple expected"]

(* Expand a tuple expression to a sequence of instructions that get the nth element *)
and expand_tuple tuple path keep_path =
  let lengths = get_tuple_lengths tuple path in
  let gets =
    let (LLTZ.R.Path.Here list) = path in
    List.mapi list ~f:(fun i num ->
      match List.nth lengths i with
      | Some length -> seq ((if keep_path then [ dup 0 ] else []) @ [ get_n num ~length ])
      | None -> raise_s [%message "Index out of bounds" (i : int) (lengths : int list)])
  in
  lengths, gets, seq ([ compile tuple ] @ gets)

(* Compile let-tuple-in expression by compiling the right-hand side with the tuple, then binding the components to the variables in the inner expression. *)
and compile_let_tuple_in expr =
  match expr.desc with
  | Let_tuple_in { components; rhs; in_ } ->
    let rhs_instr = compile rhs in
    let new_env = List.map components ~f:(fun (Var var) -> `Ident var) in
    trace
      ~flag:"let tuple in"
      (seq
         [ rhs_instr
         ; unpair_n (List.length components)
         ; trace
             (Slot.let_all
                new_env
                ~unused_set:(unused_set expr)
                ~in_:(seq [ compile in_ ]))
         ])
  | _ -> assert false

(* Compile lambda expression by compiling the body and creating a lambda instruction. *)
and compile_lambda expr =
  match expr.desc with
  | Lambda { lam_var = Var var, lam_var_type; body } ->
    let lam_var = var, convert_type lam_var_type in
    let return_type = convert_type body.type_ in
    let environment = LLTZ.Free_vars.free_vars_with_types expr in
    let partial_apps =
      seq
        (List.map (environment |> Map.to_alist) ~f:(fun (ident, var_ty) ->
           seq
             [ Slot.dup_or_dig
                 (`Ident ident)
                 (LLTZ.T.is_duppable var_ty
                  && Stdlib.not (Set.mem expr.annotations.last_used_vars ident))
             ; apply
             ]))
    in
    seq
      ([ lambda
           ~environment:(environment |> Map.map ~f:convert_type |> Map.to_alist)
           ~lam_var
           ~return_type
           (compile body)
       ]
       @ [ partial_apps ])
  | _ -> assert false

(* Compile lambda-rec expression by compiling the body and creating a lambda-rec instruction. *)
and compile_lambda_rec expr =
  match expr.desc with
  | Lambda_rec { mu_var = Var mu, _; lambda = { lam_var = Var var, lam_var_type; body } }
    ->
    let lam_var = var, convert_type lam_var_type in
    let return_type = convert_type body.type_ in
    let environment = LLTZ.Free_vars.free_vars_with_types expr in
    let partial_apps =
      seq
        (List.map (environment |> Map.to_alist) ~f:(fun (ident, var_ty) ->
           seq
             [ Slot.dup_or_dig
                 (`Ident ident)
                 (LLTZ.T.is_duppable var_ty
                  && Stdlib.not (Set.mem expr.annotations.last_used_vars ident))
             ; apply
             ]))
    in
    seq
      ([ lambda_rec
           ~environment:(environment |> Map.map ~f:convert_type |> Map.to_alist)
           ~lam_var
           ~mu
           ~return_type
           ~partial_apps
           (compile body)
       ]
       @ [ partial_apps ])
  | _ -> assert false

(* Compile an application by compiling a lambda and argument, then applying the EXEC instruction. *)
and compile_app abs arg =
  Instruction.seq
    ([ trace (compile arg); trace (compile abs) ] @ [ dig 1; trace ~flag:"exec" exec ])

(* Compile contract creation expression by compiling the delegate, initial balance, and initial storage, applying CREATE_CONTRACT instruction. *)
and compile_create_contract
  _storage
  binder_var
  binder_ty
  code_body
  delegate
  initial_balance
  initial_storage
  =
  match binder_ty.desc with
  | LLTZ.T.Tuple (LLTZ.R.Node [ LLTZ.R.Leaf (_, param); LLTZ.R.Leaf (_, storage) ]) ->
    let storage_ty = convert_type storage in
    let param_ty = convert_type param in
    let code_instr =
      seq
        [ Slot.let_
            (`Ident binder_var)
            ~unused_set:(unused_set code_body)
            ~in_:(compile code_body)
        ]
    in
    seq
      [ compile initial_storage
      ; compile initial_balance
      ; compile delegate
      ; create_contract ~storage:storage_ty ~parameter:param_ty ~code:(fun stack ->
          M.seq (code_instr stack).instructions)
      ; pair
      ]
  | _ -> raise_s [%message "Tuple expected"]

(* Compile for-each expression by compiling the collection, then applying the ITER instruction that iterates over the collection and binds the values to the variables in the body. *)
and compile_for_each expr =
  match expr.desc with
  | For_each { collection; body = { lam_var = Var var, _; body = body } } ->
    let coll_instr = compile collection in
    trace
      ~flag:"for_each "
      (seq
         [ coll_instr
         ; iter
             (seq
                [ Slot.let_
                    (`Ident var)
                    ~unused_set:(unused_set body)
                    ~in_:(seq [ compile body; drop 1 ])
                ])
         ; remove_last_used expr
         ; unit
         ])
  | _ -> assert false

(* Compile map expression by compiling the collection, then applying the MAP instruction that maps over the collection and binds the values to the variables in the function body. *)
and compile_map collection var lam_body =
  let coll_instr = compile collection in
  seq
    [ coll_instr
    ; map_
        (seq
           [ Slot.let_
               (`Ident var)
               ~unused_set:(unused_set lam_body)
               ~in_:(compile lam_body)
           ])
    ]

(* Compile fold-left expression by compiling the collection, initial value, and body, then applying the ITER instruction that iterates over the collection and binds the values to the variables in the function body. *)
and compile_fold_left collection init_body var fold_body =
  let coll_instr = compile collection in
  let init_instr = compile init_body in
  seq
    [ init_instr
    ; coll_instr
    ; iter
        (seq
           [ swap
           ; pair
           ; (* Creates pair (acc, val) *)
             Slot.let_
               (`Ident var)
               ~unused_set:(unused_set fold_body)
               ~in_:(compile fold_body)
           ])
    ]

(* Compile fold-right expression by compiling the collection, initial value, and body, then applying the ITER instruction that iterates over the collection and binds the values to the variables in the function body. *)
and compile_fold_right collection init_body var fold_body =
  let coll_instr = compile collection in
  let init_instr = compile init_body in
  seq
    [ init_instr
    ; coll_instr
    ; nil (convert_type (get_coll_elem_type collection))
    ; swap
    ; iter (seq [ cons ])
    ; iter
        (seq
           [ pair
           ; (* Creates pair (val, acc) *)
             Slot.let_
               (`Ident var)
               ~unused_set:(unused_set fold_body)
               ~in_:(compile fold_body)
           ])
    ]

and get_coll_elem_type collection =
  match collection.type_.desc with
  | List ty -> ty
  | Set ty -> ty
  | Map (key, value) ->
    LLTZ.Dsl.tuple_ty
      ~range:collection.range
      (LLTZ.R.Node [ LLTZ.R.Leaf (None, key); LLTZ.R.Leaf (None, value) ])
  | _ -> raise_s [%message "Collection expected"]

and compile_inj context expr =
  (*crete or type*)
  (*middle type*)
  match context with
  | LLTZ.R.Context.Hole ty ->
    let mid_ty = convert_type ty in
    seq [ compile expr; left mid_ty ]
  | LLTZ.R.Context.Node (left_val, mid, right_val) ->
    let right_ty = Type.ors (List.map ~f:compile_row_types_for_or right_val) in
    let mid_ty = Type.ors (List.map ~f:compile_row_types_for_or [ mid ]) in
    (*go(fold) through all elements in left (<- direction) and righ-comb iteratively merge them
      into a larger or type, record each intermediate merge*)
    let right_instrs_types =
      List.fold_right
        left_val
        ~f:(fun x lst ->
          match lst with
          | hd :: tl -> Type.ors [ compile_row_types_for_or x; hd ] :: hd :: tl
          | [] -> raise_s [%message "Empty list"])
        ~init:[ Type.ors [ mid_ty; right_ty ] ]
    in
    trace
      ~flag:"inj"
      (seq
         ([ compile expr; (* Left *) left mid_ty ]
          (* Rights - traverses all right_instrs_types in reverse order except last and makes right*)
          @
          match right_instrs_types with
          | [] -> []
          | _ :: tl -> List.map (List.rev tl) ~f:(fun ty -> right ty)))

and compile_row_of_lambdas row =
  match row with
  | LLTZ.R.Node nodes ->
    let compiled_nodes = List.map nodes ~f:compile_row_of_lambdas in
    Instruction.seq (compiled_nodes @ [ Instruction.pair_n (List.length compiled_nodes) ])
  | LLTZ.R.Leaf (_, LLTZ.E.{ lam_var = Var var, var_type; body }) ->
    compile (LLTZ.Dsl_with_dummy.lambda (Var var, var_type) ~body)

and compile_match subject cases =
  (* subject is a result of Inj *)
  let subject_instr = compile subject in
  (* Compile subject, then unwrap it and apply corresponding lambda *)
  trace ~flag:"match" (seq [ subject_instr; compile_matching cases ])

and compile_matching cases =
  match cases with
  | LLTZ.R.Node nodes ->
    (match nodes with
     | hd :: tl ->
       seq
         [ if_left ~left:(compile_matching hd) ~right:(compile_matching (LLTZ.R.Node tl))
         ]
     | [] -> seq [])
  | LLTZ.R.Leaf (_, { lam_var = Var var, var_type; body }) ->
    seq [ compile (LLTZ.Dsl_with_dummy.lambda (Var var, var_type) ~body); exec ]
(*seq [ Slot.let_ (`Ident var) ~in_:(compile  body); ]*)

and compile_raw_michelson michelson args =
  let michelson =
    Micheline.map_node
      (fun _ -> ())
      (fun prim -> Michelson.Ast.Prim.of_string prim)
      michelson
  in
  let args_instrs = List.map ~f:compile args in
  trace
    ~flag:"raw_michelson"
    (seq (List.rev_append args_instrs [ raw_michelson michelson args ]))

and compile_global_constant hash args return_ty =
  let args_instrs = List.map ~f:compile args in
  match args with
  | [] ->
    (match return_ty with
     | { desc = LLTZ.T.Function (parameter_type, return_type); _ } ->
       seq
         [ lambda_raw
             ~parameter_type:(convert_type parameter_type)
             ~return_type:(convert_type return_type)
             (seq [ global_constant hash [] ])
         ]
     | _ -> global_constant hash [])
  | _ -> seq (List.rev_append args_instrs [ global_constant hash args ])
;;

(* Compile and additionally convert to a single micheline node *)
let compile_to_micheline ?(optimize = true) ?(strip_annots = true) expr stack =
  let expr = Last_vars.compute_last_vars expr in
  let compiled = compile expr in
  let micheline = Michelson.Ast.seq (compiled stack).instructions in
  if optimize
  then Michelson_optimisations.Rewriter.optimise_micheline ~strip_annots micheline
  else micheline
;;

let compile_contract_to_micheline
  ?(optimize = true)
  ?(strip_annots = true)
  input_var
  expr
  stack
  =
  let expr = Last_vars.compute_last_vars expr in
  let compiled = compile_contract input_var expr in
  let micheline = Michelson.Ast.seq (compiled stack).instructions in
  if optimize
  then Michelson_optimisations.Rewriter.optimise_micheline ~strip_annots micheline
  else micheline
;;
