(*
   lltz_michelson.ml
   Compiles types, constants, primitives and expressions from LLTZ-IR to Michelson Ast.
*)

module LLTZ = struct
  module E = Lltz_ir.Expr
  module T = Lltz_ir.Type
  module R = Lltz_ir.Row
  module P = Lltz_ir.Primitive
end

module Michelson = struct
  module Ast = Michelson.Ast
  module T = Michelson.Ast.Type
end

module Instruction = Instruction

let rec convert_type (ty : LLTZ.T.t) : Michelson.Ast.t =
  match ty.desc with
  | Tuple row -> Michelson.T.unit (* TODO: ~M.Type.pair (List.map row ~f:convert_type)*)
  | Or row -> Michelson.T.unit (*TODO: ~M.Type.or_ (List.map row ~f:convert_type)*)
  | Option ty -> Michelson.T.option (convert_type ty)
  | List ty -> Michelson.T.list (convert_type ty)
  | Set ty -> Michelson.T.set (convert_type ty)
  | Contract ty -> Michelson.T.contract (convert_type ty)
  | Ticket ty -> Michelson.T.ticket (convert_type ty)
  | Function (param, ret) -> Michelson.T.lambda (convert_type param) (convert_type ret)
  | Map (key, value) -> Michelson.T.map (convert_type key) (convert_type value)
  | Big_map (key, value) -> Michelson.T.big_map (convert_type key) (convert_type value)
  | Unit -> Michelson.T.unit
  | Bool -> Michelson.T.bool
  | Nat -> Michelson.T.nat
  | Int -> Michelson.T.int
  | Mutez -> Michelson.T.mutez
  | String -> Michelson.T.string
  | Bytes -> Michelson.T.bytes
  | Chain_id -> Michelson.T.chain_id
  | Timestamp -> Michelson.T.timestamp
  | Address -> Michelson.T.address
  | Keys -> Michelson.T.key
  | Key_hash -> Michelson.T.key_hash
  | Signature -> Michelson.T.signature
  | Operation -> Michelson.T.operation
  | Sapling_state { memo } -> Michelson.T.sampling_state (Michelson.Ast.int memo)
  | Sapling_transaction { memo } ->
    Michelson.T.sapling_transaction (Michelson.Ast.int memo)
  | Never -> Michelson.T.never
  | Bls12_381_g1 -> Michelson.T.bls12_381_g1
  | Bls12_381_g2 -> Michelson.T.bls12_381_g2
  | Bls12_381_fr -> Michelson.T.bls12_381_fr
  | Chest_key -> Michelson.T.chest_key
  | Chest -> Michelson.T.chest

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
  | Timestamp s -> Michelson.Ast.string s
  | Bls12_381_g1 s -> Michelson.Ast.string s
  | Bls12_381_g2 s -> Michelson.Ast.string s
  | Bls12_381_fr s -> Michelson.Ast.string s
  | Signature s -> Michelson.Ast.string s

let get_const_type (const : LLTZ.E.constant) : Michelson.Ast.t =
  match const with
  | Unit -> Michelson.T.unit
  | Bool _ -> Michelson.T.bool
  | Nat _ -> Michelson.T.nat
  | Int _ -> Michelson.T.int
  | Mutez _ -> Michelson.T.mutez
  | String _ -> Michelson.T.string
  | Key _ -> Michelson.T.key
  | Key_hash _ -> Michelson.T.key_hash
  | Bytes _ -> Michelson.T.bytes
  | Chain_id _ -> Michelson.T.chain_id
  | Address _ -> Michelson.T.address
  | Timestamp _ -> Michelson.T.timestamp
  | Bls12_381_g1 _ -> Michelson.T.bls12_381_g1
  | Bls12_381_g2 _ -> Michelson.T.bls12_381_g2
  | Bls12_381_fr _ -> Michelson.T.bls12_381_fr
  | Signature _ -> Michelson.T.signature

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
    | Sapling_empty_state { memo } -> sapling_empty_state (Michelson.Ast.int memo)
    | Unit -> unit
    | Car -> car
    | Cdr -> cdr
    | Left (opt1, opt2, ty) -> left (convert_type ty) (* TODO: resolve tag options *)
    | Right (opt1, opt2, ty) -> right (convert_type ty) (* TODO: resolve tag options *)
    | Some -> some
    | Eq -> eq
    | Abs -> abs
    | Neg -> neg
    | Nat -> int
    | Int -> int
    | Bytes -> pack (* Assuming pack handles bytes conversion *)
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
    | Contract (opt, ty) -> contract (convert_type ty) (* TODO: resolve tag option*)
    | Pack -> pack
    | Unpack ty -> unpack (convert_type ty)
    | Hash_key -> hash_key
    | Blake2b -> blake2b
    | Sha256 -> sha256
    | Sha512 -> sha512
    | Keccak -> keccak
    | Sha3 -> sha3
    | Set_delegate -> set_delegate
    | Read_ticket -> read_ticket
    | Join_tickets -> join_tickets
    | Pairing_check -> pairing_check
    | Voting_power -> voting_power
    | Getn n -> get_n n
    | Cast ty -> cast (convert_type ty)
    | Rename opt -> failwith (* TODO: Check why the instruction does not exist. *)
    | Emit (opt, ty_opt) -> emit opt (Option.map convert_type ty_opt)
    | Failwith -> failwith
    | Never -> never
    | Pair (opt1, opt2) -> pair (* TODO: resolve tag options*)
    | Add -> add
    | Mul -> mul
    | Sub -> sub
    | Sub_mutez -> sub
    | Lsr -> lsr_
    | Lsl -> lsl_
    | Xor -> xor
    | Ediv -> ediv
    | And -> and_
    | Or -> or_
    | Cons -> cons
    | Compare -> compare
    | Concat1 -> concat
    | Concat2 -> concat
    | Get -> get
    | Mem -> mem
    | Exec -> exec
    | Apply -> apply
    | Sapling_verify_update -> sapling_verify_update
    | Ticket -> ticket
    | Ticket_deprecated -> ticket_deprecated
    | Split_ticket -> split_ticket
    | Updaten n -> update_n n
    | View (name, ty) -> view name (convert_type ty)
    | Slice -> slice
    | Update -> update
    | Get_and_update -> get_and_update
    | Transfer_tokens -> transfer_tokens
    | Check_signature -> check_signature
    | Open_chest -> open_chest

let rec compile : LLTZ.E.t -> Instruction.t = fun expr ->
  Instruction.seq [
    match expr.desc with
    | Variable (Var name) -> compile_variable name
    | Let_in { let_var = Var var; rhs; in_ } ->
        compile_let_in var rhs in_
    | Lambda { lam_var = (Var var, lam_var_type); return_type; body } -> 
        assert false
    | Lambda_rec { lam_var = (Var var, lam_var_type); mu_var = Var mu; return_type; body } -> 
        assert false
    | App { abs; arg } ->
        assert false
    | Const constant -> 
        compile_const constant
    | Prim (primitive, args) ->
        compile_prim primitive args
    | Let_mut_in { let_var = Mut_var var; rhs; in_ } ->
        assert false
    | Deref (Mut_var var) ->
        compile_deref var
    | Assign (Mut_var var, value) ->
        compile_assign var value
    | If_bool { condition; if_true; if_false } ->
        compile_if_bool condition if_true if_false
    | If_none { subject; if_none; if_some = (Var var, some) } ->
        compile_if_none subject if_none (var, some)
    | If_cons { subject; if_empty; if_nonempty = (Var hd, Var tl, nonempty) } ->
        compile_if_cons subject if_empty (hd, tl, nonempty)
    | If_left { subject; if_left = (Var left, l); if_right = (Var right, r) } ->
        compile_if_left subject (left, l) (right, r)
    | While { invariant; body } ->
        compile_while invariant body
    | While_left { invariant; body } ->
        compile_while_left invariant body
    | For { index = Mut_var var; init; invariant; variant; body } -> 
        compile_for var init invariant variant body
    | For_each { indices; collection; body } -> 
        assert false
    | Map { collection; map = (vars, body) } -> 
        assert false
    | Fold_left { collection; init = (Var acc, init_body); fold = (Var var, fold_body) } -> 
        assert false
    | Fold_right { collection; init = (Var acc, init_body); fold = (Var var, fold_body) } -> 
        assert false
    | Let_tuple_in { components; rhs; in_ } -> 
        assert false
    | Tuple row ->
        assert false
    | Proj (tuple, path) ->
        assert false
    | Update { tuple; component; update } ->
        assert false
    | Inj (path, expr) ->
        assert false
    | Match (subject, cases) ->
        assert false
    | Raw_michelson node ->
        assert false
    | Create_contract { storage; parameter; code; delegate; initial_balance; initial_storage } -> 
        assert false
  ]

(* Compile a variable by duplicating its value on the stack. *)  
and compile_variable (name : string) =
  Instruction.Slot.dup (`Ident name)

(* Compile a let-in expression by compiling the right-hand side, then binding the result to the variable in the inner expression. *)
and compile_let_in (var : string) (rhs : LLTZ.E.t) (in_ : LLTZ.E.t) =
  Instruction.seq [
    compile rhs;
    Instruction.Slot.let_ (`Ident var) ~in_:(compile in_)
  ]

(* Compile a constant by pushing its value onto the stack. *)
and compile_const constant =
  Instruction.seq [
    Instruction.push (get_const_type constant) (convert_constant constant)
  ]

(* Compile a primitive by compiling its arguments, then applying the primitive to the arguments. *)
and compile_prim primitive args =
  let args_instrs = List.map compile args in
  Instruction.seq (
    args_instrs @ [ Instruction.prim (List.length args) 1 (convert_primitive primitive) ]
  )

(* Compile a dereference by duplicating the value of the mutable variable on the stack. *)
and compile_deref (var : string) =
  Instruction.Slot.dup (`Ident var)

(* Compile an assignment by compiling the value to be assigned, then assigning it to the slot corresponding to the mutable variable. *)
and compile_assign (var : string) value =
  Instruction.trace (
    Instruction.seq [
      Instruction.trace (compile value);
      Instruction.Slot.set (`Ident var)
    ]
  )

(* Compile an if-bool expression by compiling the condition, then applying the if-bool instruction to the condition and the true and false branches. *)
and compile_if_bool condition if_true if_false =
  Instruction.seq [
    compile condition;
    Instruction.if_ ~then_:(compile if_true) ~else_:(compile if_false)
  ]

(* Compile an if-none expression by compiling the subject, then applying the if-none instruction to the subject and the none and some branches. *)
and compile_if_none subject if_none (var, some) =
  Instruction.seq [
    compile subject;
    Instruction.if_none ~none:(compile if_none) ~some:(Instruction.Slot.let_ (`Ident var) ~in_:(compile some))
  ]

(* Compile an if-cons expression by compiling the subject, then applying the if-cons instruction to the subject and the empty and nonempty branches. *)
and compile_if_cons subject if_empty (hd, tl, nonempty) =
  Instruction.trace (
    Instruction.seq [
      compile subject;
      Instruction.if_cons ~empty:(compile if_empty) ~nonempty:(Instruction.Slot.let_all [ `Ident hd; `Ident tl ] ~in_:(compile nonempty))
    ]
  )

(* Compile an if-left expression by compiling the subject, then applying the if-left instruction to the subject and the left and right branches. *)
and compile_if_left subject (left, l) (right, r) =
  Instruction.seq [
    compile subject;
    Instruction.if_left ~left:(Instruction.Slot.let_ (`Ident left) ~in_:(compile l)) ~right:(Instruction.Slot.let_ (`Ident right) ~in_:(compile r))
  ]

(* Compile a while expression by compiling the invariant, then applying the loop instruction to the body and invariant. *)
and compile_while invariant body =
  Instruction.seq [
    compile invariant;
    Instruction.loop (Instruction.seq [ compile body; compile invariant ])
  ]

(* Compile a while-left expression by compiling the invariant, then applying the loop-left instruction to the body and invariant. *)
and compile_while_left invariant body =
  Instruction.seq [
    compile invariant;
    Instruction.loop_left (Instruction.seq [ compile body; compile invariant ])
  ]

(* Compile a for expression by compiling the initial value, invariant, variant, and body, 
   then applying the loop to the sequence of body, variant, and invariant. *)
and compile_for index init invariant variant body =
  let init_instr = compile init in
  let inv_instr = compile invariant in
  Instruction.seq [
    init_instr;
    inv_instr;
    Instruction.loop (Instruction.seq [ Instruction.Slot.let_ (`Ident index) ~in_:(compile body); compile variant; inv_instr ]);
    Instruction.drop 1 (*drop initial value*)
  ]

