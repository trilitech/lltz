(*
   lltz_michelson.ml
   Compiles types, constants, primitives and expressions from LLTZ-IR to Michelson Ast.
*)

module Stack = Stack
module Type = Type
module Instruction = Instruction
module Slot = Slot
open Core

module LLTZ = struct
  module E = Lltz_ir.Expr
  module T = Lltz_ir.Type
  module R = Lltz_ir.Row
  module P = Lltz_ir.Primitive
  module Dsl = Lltz_ir.Dsl
end

module Michelson = struct
  module Ast = Michelson.Ast
  module T = Michelson.Ast.Type
end

open Instruction

(* Creates corresponding tuple type from a row of types *)
let rec compile_row_types row =
  match row with
  | LLTZ.R.Node nodes -> Type.tuple (List.map nodes ~f:compile_row_types)
  | LLTZ.R.Leaf (_, value) -> convert_type value

(* Creates corresponding or type from a row of types *)
and compile_row_types_for_or row =
  match row with
  | LLTZ.R.Node nodes ->
    let converted_types = List.map nodes ~f:compile_row_types in
    Type.ors converted_types
  | LLTZ.R.Leaf (_, value) -> convert_type value

and convert_type (ty : LLTZ.T.t) : Michelson.Ast.t =
  match ty.desc with
  | Tuple row -> compile_row_types row
  | Or row -> compile_row_types_for_or row
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
;;

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
  | Get_n n -> get_n n
  | Cast ty -> cast (convert_type ty)
  | Rename opt -> failwith (* TODO: Check why the instruction does not exist. *)
  | Emit (opt, ty_opt) -> emit opt (Option.map ~f:convert_type ty_opt)
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
  | Update_n n -> update_n n
  | View (name, ty) -> view name (convert_type ty)
  | Slice -> slice
  | Update -> update
  | Get_and_update -> get_and_update
  | Transfer_tokens -> transfer_tokens
  | Check_signature -> check_signature
  | Open_chest -> open_chest
;;

let rec compile : LLTZ.E.t -> t =
  fun expr ->
  seq
    [ (match expr.desc with
       | Variable (Var name) -> compile_variable name
       | Let_in { let_var = Var var; rhs; in_ } -> compile_let_in var rhs in_
       | Lambda { lam_var = Var var, lam_var_type; body } ->
         compile_lambda var lam_var_type body
       | Lambda_rec
           { mu_var = Var mu, mu_type
           ; lambda = { lam_var = Var var, lam_var_type; body }
           } -> compile_lambda_rec var lam_var_type mu body
       | App { abs; arg } -> compile_app abs arg
       | Const constant -> compile_const constant
       | Prim (primitive, args) -> compile_prim primitive args
       | Let_mut_in { let_var = Mut_var var; rhs; in_ } -> assert false
       | Deref (Mut_var var) -> compile_deref var
       | Assign (Mut_var var, value) -> compile_assign var value
       | If_bool { condition; if_true; if_false } ->
         compile_if_bool condition if_true if_false
       | If_none
           { subject; if_none; if_some = { lam_var = Var var, var_type; body = some } } ->
         compile_if_none subject if_none (var, some)
       | If_cons
           { subject
           ; if_empty
           ; if_nonempty =
               { lam_var1 = Var hd, var1_ty; lam_var2 = Var tl, var2_ty; body = nonempty }
           } -> compile_if_cons subject if_empty (hd, tl, nonempty)
       | If_left
           { subject
           ; if_left = { lam_var = Var left, left_ty; body = l }
           ; if_right = { lam_var = Var right, right_ty; body = r }
           } -> compile_if_left subject (left, l) (right, r)
       | While { cond; body } -> compile_while cond body
       | While_left { cond; body = { lam_var = Var var, var_ty; body = body_lambda } } ->
         compile_while_left cond var body_lambda
       | For { index = Mut_var var; init; cond; update; body } ->
         compile_for var init cond update body
       | For_each { collection; body = { lam_var = Var var, var_ty; body = lambda_body } }
         -> compile_for_each collection var lambda_body
       | Map { collection; map = { lam_var = Var var, var_ty; body = lam_body } } ->
         compile_map collection var lam_body
       | Fold_left
           { collection
           ; init = init_body
           ; fold = { lam_var = Var var, var_ty; body = fold_body }
           } -> compile_fold_left collection init_body var fold_body
       | Fold_right
           { collection
           ; init = init_body
           ; fold = { lam_var = Var var, var_ty; body = fold_body }
           } -> compile_fold_right collection init_body var fold_body
       | Let_tuple_in { components; rhs; in_ } -> compile_let_tuple_in components rhs in_
       | Tuple row -> compile_tuple row
       | Proj (tuple, path) -> compile_proj tuple path
       | Update { tuple; component; update } -> compile_update tuple component update
       | Inj (path, expr) -> compile_inj path expr
       | Match (subject, cases) -> compile_match subject cases
       | Raw_michelson node -> assert false
       | Create_contract
           { storage
           ; code = { lam_var = Var param_var, param_ty; body = code_body }
           ; delegate
           ; initial_balance
           ; initial_storage
           } ->
         compile_create_contract
           storage
           param_var
           param_ty
           code_body
           delegate
           initial_balance
           initial_storage
       | Global_constant hash -> assert false)
    ]

(* Compile a variable by duplicating its value on the stack. *)
and compile_variable (name : string) = Slot.dup (`Ident name)

(* Compile a let-in expression by compiling the right-hand side, then binding the result to the variable in the inner expression. *)
and compile_let_in (var : string) (rhs : LLTZ.E.t) (in_ : LLTZ.E.t) =
  seq [ compile rhs; Slot.let_ (`Ident var) ~in_:(compile in_) ]

(* Compile a constant by pushing its value onto the stack. *)
and compile_const constant =
  seq [ push (get_const_type constant) (convert_constant constant) ]

(* Compile a primitive by compiling its arguments, then applying the primitive to the arguments. *)
(* TODO: READ_TICKET, *)
and compile_prim primitive args =
  let args_instrs = List.map ~f:compile args in
  seq (args_instrs @ [ prim (List.length args) 1 (convert_primitive primitive) ])

(* Compile a dereference by duplicating the value of the mutable variable on the stack. *)
and compile_deref (var : string) = Slot.dup (`Ident var)

(* Compile an assignment by compiling the value to be assigned, then assigning it to the slot corresponding to the mutable variable. *)
and compile_assign (var : string) value =
  trace (seq [ trace (compile value); Slot.set (`Ident var) ])

(* Compile an if-bool expression by compiling the condition, then applying the if-bool instruction to the condition and the true and false branches. *)
and compile_if_bool condition if_true if_false =
  seq [ compile condition; if_ ~then_:(compile if_true) ~else_:(compile if_false) ]

(* Compile an if-none expression by compiling the subject, then applying the if-none instruction to the subject and the none and some branches. *)
and compile_if_none subject if_none_clause (var, some_clause) =
  seq
    [ compile subject
    ; if_none
        ~none:(compile if_none_clause)
        ~some:(Slot.let_ (`Ident var) ~in_:(compile some_clause))
    ]

(* Compile an if-cons expression by compiling the subject, then applying the if-cons instruction to the subject and the empty and nonempty branches. *)
and compile_if_cons subject if_empty (hd, tl, nonempty) =
  trace
    (seq
       [ compile subject
       ; if_cons
           ~empty:(compile if_empty)
           ~nonempty:(Slot.let_all [ `Ident hd; `Ident tl ] ~in_:(compile nonempty))
       ])

(* Compile an if-left expression by compiling the subject, then applying the if-left instruction to the subject and the left and right branches. *)
and compile_if_left subject (left, l) (right, r) =
  seq
    [ compile subject
    ; if_left
        ~left:(Slot.let_ (`Ident left) ~in_:(compile l))
        ~right:(Slot.let_ (`Ident right) ~in_:(compile r))
    ]

(* Compile a while expression by compiling the invariant, then applying the loop instruction to the body and invariant. *)
and compile_while invariant body =
  seq [ compile invariant; loop (seq [ compile body; compile invariant ]) ]

(* Compile a while-left expression by compiling the invariant, then applying the loop-left instruction to the body and invariant. *)
and compile_while_left invariant var body_lambda =
  seq
    [ compile invariant
    ; loop_left
        (seq [ Slot.let_ (`Ident var) ~in_:(compile body_lambda); compile invariant ])
    ]

(* Compile a for expression by compiling the initial value, invariant, variant, and body,
   then applying the loop to the sequence of body, variant, and invariant. *)
and compile_for index init invariant variant body =
  let init_instr = compile init in
  let inv_instr = compile invariant in
  seq
    [ init_instr
    ; inv_instr
    ; loop
        (seq [ Slot.let_ (`Ident index) ~in_:(compile body); compile variant; inv_instr ])
    ; drop 1 (*drop initial value*)
    ]

(* Compile a tuple expression by compiling each component and pairing them together. *)
and compile_tuple row =
  match row with
  | LLTZ.R.Node nodes ->
    let compiled_nodes = List.map ~f:compile_tuple nodes in
    seq (compiled_nodes @ [ pair_n (List.length compiled_nodes) ])
  | LLTZ.R.Leaf (_, value) -> compile value

(* Compile a projection expression by compiling the tuple and then getting the nth element. *)
and compile_proj tuple path =
  let _, gets, tuple_expanded_instr = expand_tuple tuple path in
  trace
    (seq
       ([ tuple_expanded_instr ]
        @ [ (* Keep the last value, drop the intermediate ones and the tuple *)
            trace (dip 1 (drop (List.length gets - 1)))
          ]))

(* Compile an update expression by compiling the tuple row, getting the nth element, compiling the update value, and combining the values back together into tuple. *)
and compile_update tuple component update =
  let lengths, gets, tuple_expanded_instr = expand_tuple tuple component in
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
  seq ([ compile tuple ] @ gets @ [ compile update ] @ updates)

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
                (nodes : LLTZ.E.t LLTZ.R.t list)])
     | [] -> [])
  | LLTZ.R.Leaf (_, _) -> [ 1 ]

(* Get the number of children for each node on the path *)
and get_tuple_lengths tuple path =
  match LLTZ.E.(tuple.desc), path with
  | LLTZ.E.Tuple row, LLTZ.R.Path.Here list -> get_lengths_inner row list
  | _ -> raise_s [%message "Tuple expected"]

(* Expand a tuple expression to a sequence of instructions that get the nth element *)
and expand_tuple tuple path =
  let lengths = get_tuple_lengths tuple path in
  let gets =
    let (LLTZ.R.Path.Here list) = path in
    List.mapi list ~f:(fun i num ->
      match List.nth lengths i with
      | Some length -> get_n num ~length
      | None -> raise_s [%message "Index out of bounds" (i : int) (lengths : int list)])
  in
  lengths, gets, seq ([ compile tuple ] @ gets)

(* Compile let-tuple-in expression by compiling the right-hand side with the tuple, then binding the components to the variables in the inner expression. *)
and compile_let_tuple_in components rhs in_ =
  let rhs_instr = compile rhs in
  let new_env = List.map components ~f:(fun (Var var) -> `Ident var) in
  seq
    [ rhs_instr
    ; unpair_n (List.length components)
    ; trace (Slot.let_all new_env ~in_:(compile in_))
    ]

(* Compile lambda expression by compiling the body and creating a lambda instruction. *)
and compile_lambda var lam_var_type body =
  let lam_var = var, convert_type lam_var_type in
  let return_type = convert_type body.type_ in
  Instruction.seq [ Instruction.lambda ~lam_var ~return_type (compile body) ]

(* Compile lambda-rec expression by compiling the body and creating a lambda-rec instruction. *)
and compile_lambda_rec var lam_var_type mu body =
  let lam_var = var, convert_type lam_var_type in
  let return_type = convert_type body.type_ in
  Instruction.seq [ Instruction.lambda_rec ~lam_var ~mu ~return_type (compile body) ]

(* Compile an application by compiling a lambda and argument, then applying the EXEC instruction. *)
and compile_app abs arg =
  trace (Instruction.seq [ trace (compile abs); trace (compile arg); exec ])

(* Compile contract creation expression by compiling the delegate, initial balance, and initial storage, applying CREATE_CONTRACT instruction. *)
and compile_create_contract
  storage
  param_var
  param_ty
  code_body
  delegate
  initial_balance
  initial_storage
  =
  let storage_ty = convert_type storage in
  let param_ty = convert_type param_ty in
  let code_instr = seq [ Slot.let_ (`Ident param_var) ~in_:(compile code_body) ] in
  seq
    [ compile delegate (*TODO possibly needs triple lambda*)
    ; compile initial_balance
    ; compile initial_storage
    ; create_contract ~storage:storage_ty ~parameter:param_ty ~code:(fun stack ->
        M.seq (code_instr stack).instructions)
    ; pair
    ]

(* Compile for-each expression by compiling the collection, then applying the ITER instruction that iterates over the collection and binds the values to the variables in the body. *)
and compile_for_each collection var body =
  let coll_instr = compile collection in
  seq [ coll_instr; iter (seq [ Slot.let_ (`Ident var) ~in_:(compile body) ]) ]

(* Compile map expression by compiling the collection, then applying the MAP instruction that maps over the collection and binds the values to the variables in the function body. *)
and compile_map collection var lam_body =
  let coll_instr = compile collection in
  seq [ coll_instr; map_ (seq [ Slot.let_ (`Ident var) ~in_:(compile lam_body) ]) ]

(* Compile fold-left expression by compiling the collection, initial value, and body, then applying the ITER instruction that iterates over the collection and binds the values to the variables in the function body. *)
and compile_fold_left collection init_body var fold_body =
  let coll_instr = compile collection in
  let init_instr = compile init_body in
  seq
    [ init_instr
    ; coll_instr
    ; iter
        (seq
           [ pair
           ; (* Creates pair (acc, val) *)
             Slot.let_ (`Ident var) ~in_:(compile fold_body)
           ])
    ]

(* Compile fold-right expression by compiling the collection, initial value, and body, then applying the ITER instruction that iterates over the collection and binds the values to the variables in the function body. *)
and compile_fold_right collection init_body var fold_body =
  let coll_instr = compile collection in
  let init_instr = compile init_body in
  seq
    [ init_instr
    ; coll_instr
    ; (*TODO: reverse the collection - once rest of the code is validated, it is easily done with dsl*)
      iter (seq [ pair; Slot.let_ (`Ident var) ~in_:(compile fold_body) ])
    ]

and compile_inj context expr =
  (* Wraps expr into or-type of shape RIGHT(RIGHT(...RIGHT(LEFT(expr)))) *)
  match context with
  | LLTZ.R.Context.Hole ty ->
    let mid_ty = convert_type ty in
    seq [ compile expr; left mid_ty ]
  | LLTZ.R.Context.Node (left_val, mid, right_val) ->
    let right_ty = Type.ors (List.map ~f:compile_row_types_for_or right_val) in
    let mid_ty = Type.ors (List.map ~f:compile_row_types_for_or [ mid ]) in
    (* Go(fold) through all elements in left part of the context (<- direction) and iteratively merge them
       into a larger right-comb or type, record each intermediate merge *)
    let right_instrs_types =
      List.fold_right
        left_val
        ~f:(fun x lst ->
          match lst with
          | hd :: tl -> Type.ors [ compile_row_types_for_or x; hd ] :: hd :: tl
          | [] -> raise_s [%message "Empty list"])
        ~init:[ Type.ors [ mid_ty; right_ty ] ]
    in
    seq
      ([ compile expr; left mid_ty ]
       (* Rights - traverses all right_instrs_types in reverse order except last and makes right instructions*)
       @
       match right_instrs_types with
       | hd :: tl -> List.map (List.rev tl) ~f:(fun ty -> right ty)
       | [] -> [])

and compile_row_of_lambdas row =
  match row with
  | LLTZ.R.Node nodes ->
    let compiled_nodes = List.map nodes ~f:compile_row_of_lambdas in
    Instruction.seq (compiled_nodes @ [ Instruction.pair_n (List.length compiled_nodes) ])
  | LLTZ.R.Leaf (_, LLTZ.E.{ lam_var = Var var, var_type; body }) ->
    compile (LLTZ.Dsl.lambda (Var var, var_type) ~body)

and compile_match subject cases =
  (* Subject is a result of Inj *)
  let subject_instr = compile subject in
  (* Compile subject, then unwrap it and apply corresponding lambda *)
  seq ([ subject_instr ] @ [ compile_matching cases ])

and compile_matching cases =
  (* Recursively create a decision tree that finds the corresponding lambda*)
  match cases with
  | LLTZ.R.Node nodes ->
    (match nodes with
     | hd :: tl ->
       seq
         [ if_left ~left:(compile_matching hd) ~right:(compile_matching (LLTZ.R.Node tl))
         ]
     | [] -> seq [])
  | LLTZ.R.Leaf (_, { lam_var = Var var, var_type; body }) ->
    seq [ compile (LLTZ.Dsl.lambda (Var var, var_type) ~body); exec ]
;;
