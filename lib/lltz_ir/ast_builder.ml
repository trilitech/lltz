open Grace
open Core

module Dummy_range = struct
  let v = Range.initial (`String { content = ""; name = Some "" })
end

module LLTZ = struct
  module E = Expr
  module T = Type
  module P = Primitive
  module R = Row
end

type var = Var of string
type mut_var = Mut_var of string

(* Creation with optional range *)
let create ~range desc type_ =
  { LLTZ.E.desc; range; type_; annotations = Annotations.empty }

let mk_type ~range (desc: LLTZ.T.desc) : LLTZ.T.t =
  { desc; range }

let rec get_type_row (row: LLTZ.E.t LLTZ.R.t) : LLTZ.T.t LLTZ.R.t =
  match row with
  | LLTZ.R.Leaf (label_opt, expr) -> LLTZ.R.Leaf (label_opt, expr.LLTZ.E.type_)
  | LLTZ.R.Node exprs -> LLTZ.R.Node (List.map ~f:get_type_row exprs)

let rec get_proj_type (row: LLTZ.T.t LLTZ.R.t) (path: int list) : LLTZ.T.t =
  match path with
  | i :: path_tail -> (
    match row with
    | LLTZ.R.Leaf (_, _) ->
      raise_s [%message "Invalid path" (path : int list)]
    | LLTZ.R.Node row_list -> (
      match List.nth row_list i with
      | Some expr -> get_proj_type expr path_tail
      | None -> raise_s [%message "Invalid path" (path : int list)]
    )
  )
  | [] -> (
    match row with
    | LLTZ.R.Leaf (_, ty) -> ty
    | LLTZ.R.Node _ -> raise_s [%message "Invalid path" (path : int list)]
  )

let get_inj_type (context: LLTZ.T.t LLTZ.R.Context.t) : LLTZ.T.desc =
  match context with
  | LLTZ.R.Context.Hole ty -> ty.LLTZ.T.desc
  | LLTZ.R.Context.Node (left_val, mid, right_val) ->
    let full = left_val @ [ mid ] @ right_val in
    LLTZ.T.Or (LLTZ.R.Node full)

let convert_option str_opt =
  Option.map ~f:(fun str -> LLTZ.R.Label str) str_opt

module Default = struct
  (* Constants *)
  let unit ~range =
    create ~range (LLTZ.E.Const (Unit)) (mk_type ~range LLTZ.T.Unit)

  let bool ~range b =
    create ~range (LLTZ.E.Const (Bool b)) (mk_type ~range LLTZ.T.Bool)

  let nat ~range n =
    create ~range (LLTZ.E.Const (Nat (Z.of_int n))) (mk_type ~range LLTZ.T.Nat)

  let int ~range n =
    create ~range (LLTZ.E.Const (Int (Z.of_int n))) (mk_type ~range LLTZ.T.Int)

  let mutez ~range n =
    create ~range (LLTZ.E.Const (Mutez (Z.of_int n))) (mk_type ~range LLTZ.T.Mutez)

  let string ~range s =
    create ~range (LLTZ.E.Const (String s)) (mk_type ~range LLTZ.T.String)

  let key ~range s =
    create ~range (LLTZ.E.Const (Key s)) (mk_type ~range LLTZ.T.Keys)

  let key_hash ~range s =
    create ~range (LLTZ.E.Const (Key_hash s)) (mk_type ~range LLTZ.T.Key_hash)

  let bytes ~range s =
    create ~range (LLTZ.E.Const (Bytes s)) (mk_type ~range LLTZ.T.Bytes)

  let chain_id ~range s =
    create ~range (LLTZ.E.Const (Chain_id s)) (mk_type ~range LLTZ.T.Chain_id)

  let address_const ~range s =
    create ~range (LLTZ.E.Const (Address s)) (mk_type ~range LLTZ.T.Address)

  let timestamp ~range s =
    create ~range (LLTZ.E.Const (Timestamp s)) (mk_type ~range LLTZ.T.Timestamp)

  let bls12_381_g1 ~range s =
    create ~range (LLTZ.E.Const (Bls12_381_g1 s)) (mk_type ~range LLTZ.T.Bls12_381_g1)

  let bls12_381_g2 ~range s =
    create ~range (LLTZ.E.Const (Bls12_381_g2 s)) (mk_type ~range LLTZ.T.Bls12_381_g2)

  let bls12_381_fr ~range s =
    create ~range (LLTZ.E.Const (Bls12_381_fr s)) (mk_type ~range LLTZ.T.Bls12_381_fr)

  let signature ~range s =
    create ~range (LLTZ.E.Const (Signature s)) (mk_type ~range LLTZ.T.Signature)

  (* Variables *)
  let var name = LLTZ.E.Var name
  let mut_var name = LLTZ.E.Mut_var name

  (* Types *)
  let unit_ty ~range = { LLTZ.T.desc = Unit; range }
  let bool_ty ~range = { LLTZ.T.desc = Bool; range }
  let nat_ty ~range = { LLTZ.T.desc = Nat; range }
  let int_ty ~range = { LLTZ.T.desc = Int; range }
  let mutez_ty ~range = { LLTZ.T.desc = Mutez; range }
  let string_ty ~range = { LLTZ.T.desc = String; range }
  let bytes_ty ~range = { LLTZ.T.desc = Bytes; range }
  let chain_id_ty ~range = { LLTZ.T.desc = Chain_id; range }
  let timestamp_ty ~range = { LLTZ.T.desc = Timestamp; range }
  let address_ty ~range = { LLTZ.T.desc = Address; range }
  let key_ty ~range = { LLTZ.T.desc = Keys; range }
  let key_hash_ty ~range = { LLTZ.T.desc = Key_hash; range }
  let signature_ty ~range = { LLTZ.T.desc = Signature; range }
  let operation_ty ~range = { LLTZ.T.desc = Operation; range }
  let sapling_state_ty ~range memo = { LLTZ.T.desc = Sapling_state { memo }; range }
  let sapling_transaction_ty ~range memo = { LLTZ.T.desc = Sapling_transaction { memo }; range }
  let bls12_381_g1_ty ~range = { LLTZ.T.desc = Bls12_381_g1; range }
  let bls12_381_g2_ty ~range = { LLTZ.T.desc = Bls12_381_g2; range }
  let bls12_381_fr_ty ~range = { LLTZ.T.desc = Bls12_381_fr; range }

  let tuple_ty ~range row = { LLTZ.T.desc = Tuple row; range }
  let or_ty ~range row = { LLTZ.T.desc = Or row; range }
  let option_ty ~range ty = { LLTZ.T.desc = Option ty; range }
  let list_ty ~range ty = { LLTZ.T.desc = List ty; range }
  let set_ty ~range ty = { LLTZ.T.desc = Set ty; range }
  let contract_ty ~range ty = { LLTZ.T.desc = Contract ty; range }
  let ticket_ty ~range ty = { LLTZ.T.desc = Ticket ty; range }
  let function_ty ~range arg ret = { LLTZ.T.desc = Function (arg, ret); range }
  let map_ty ~range key value = { LLTZ.T.desc = Map (key, value); range }
  let big_map_ty ~range key value = { LLTZ.T.desc = Big_map (key, value); range }

  let mk_row row = LLTZ.R.Node row

  let mk_tuple_ty ~range list =
    tuple_ty ~range (mk_row (List.map ~f:(fun x -> LLTZ.R.Leaf (None, x)) list))

  (* Primitives *)

  (* Expressions *)
  let variable ~range var var_ty =
    create ~range (LLTZ.E.Variable var) var_ty

  let let_in ~range var ~rhs ~in_ =
    create ~range (LLTZ.E.Let_in { let_var = var; rhs; in_ }) in_.LLTZ.E.type_

  let ( let* ) (rhs:LLTZ.E.t) in_ =
    let xname = Name.create () in
    let_in ~range:rhs.range (Var xname) ~rhs ~in_

  let lambda ~range (var,var_ty) ~body =
    create ~range (LLTZ.E.Lambda { lam_var = (var,var_ty); body })
      (mk_type ~range (LLTZ.T.Function (var_ty, body.LLTZ.E.type_)))

  let lambda_rec ~range mu_var (var, var_ty) ~body =
    create ~range
      (LLTZ.E.Lambda_rec {
         mu_var = (mu_var, mk_type ~range (LLTZ.T.Function (var_ty, body.LLTZ.E.type_)));
         lambda = { lam_var = (var, var_ty); body }
       })
      (mk_type ~range (LLTZ.T.Function (var_ty, body.LLTZ.E.type_)))

  let app ~range abs arg =
    let ret_ty =
      match abs.LLTZ.E.type_.LLTZ.T.desc with
      | LLTZ.T.Function (_, ret_ty) -> ret_ty
      | _ -> raise_s [%message "Expected function type"]
    in
    create ~range (LLTZ.E.App { abs; arg }) ret_ty

  let let_mut_in ~range var ~rhs ~in_ =
    create ~range (LLTZ.E.Let_mut_in { let_var = var; rhs; in_ }) in_.LLTZ.E.type_

  let deref ~range var var_ty =
    create ~range (LLTZ.E.Deref var) var_ty

  let assign ~range var value =
    create ~range (LLTZ.E.Assign (var, value)) (unit_ty ~range)

  let if_bool ~range condition ~then_ ~else_ =
    create ~range (LLTZ.E.If_bool { condition; if_true = then_; if_false = else_ })
      then_.LLTZ.E.type_

  let if_none ~range subject ~none ~some =
    create ~range (LLTZ.E.If_none { subject; if_none = none; if_some = some })
      some.body.LLTZ.E.type_

  let if_cons ~range subject ~empty ~nonempty =
    create ~range (LLTZ.E.If_cons { subject; if_empty = empty; if_nonempty = nonempty })
      nonempty.body.LLTZ.E.type_

  let if_left ~range subject ~left ~right =
    create ~range (LLTZ.E.If_left { subject; if_left = left; if_right = right })
      left.body.LLTZ.E.type_

  let while_ ~range cond ~body =
    create ~range (LLTZ.E.While { cond; body }) (unit_ty ~range)

  let while_left ~range cond ~body =
    create ~range (LLTZ.E.While_left { cond; body })
      (match body.body.LLTZ.E.type_.LLTZ.T.desc with
       | LLTZ.T.Or (LLTZ.R.Node [LLTZ.R.Leaf (_, left_ty); LLTZ.R.Leaf (_, right_ty)]) ->
         right_ty
       | _ -> raise_s [%message "Expected or type"]
      )

  let for_ ~range index ~init ~cond ~update ~body =
    create ~range (LLTZ.E.For { index; init; cond; update; body }) body.LLTZ.E.type_

  let for_each ~range collection ~body =
    create ~range (LLTZ.E.For_each { collection; body }) (unit_ty ~range)

  let map ~range collection ~map =
    create ~range (LLTZ.E.Map { collection; map })
      (match collection.LLTZ.E.type_.LLTZ.T.desc with
       | LLTZ.T.List _ -> mk_type ~range (LLTZ.T.List map.body.LLTZ.E.type_)
       | LLTZ.T.Option _ -> mk_type ~range (LLTZ.T.Option map.body.LLTZ.E.type_)
       | LLTZ.T.Map (kty, _) -> mk_type ~range (LLTZ.T.Map (kty, map.body.LLTZ.E.type_))
       | _ -> raise_s [%message "Expected list, option, or map type"]
      )

  let fold_left ~range collection ~init ~fold =
    create ~range (LLTZ.E.Fold_left { collection; init; fold }) fold.body.LLTZ.E.type_

  let fold_right ~range collection ~init ~fold =
    create ~range (LLTZ.E.Fold_right { collection; init; fold }) fold.body.LLTZ.E.type_

  let let_tuple_in ~range components ~rhs ~in_ =
    create ~range (LLTZ.E.Let_tuple_in { components; rhs; in_ }) in_.LLTZ.E.type_

  let tuple ~range row =
    create ~range (LLTZ.E.Tuple row)
      (mk_type ~range (LLTZ.T.Tuple (get_type_row row)))

  let proj ~range tuple ~path =
    create ~range (LLTZ.E.Proj (tuple, path))
      (match tuple, path with
       | { LLTZ.E.type_ = { LLTZ.T.desc = Tuple tuple_row; _ }; _ }, Here path_list ->
         get_proj_type tuple_row path_list
       | _ -> raise_s [%message "Expected tuple type"]
      )

  let update_tuple ~range tuple ~component ~update =
    create ~range (LLTZ.E.Update { tuple; component; update }) tuple.LLTZ.E.type_

  let inj ~range context expr =
    create ~range (LLTZ.E.Inj (context, expr))
      (mk_type ~range (get_inj_type context))

  let match_ ~range subject ~cases =
    create ~range (LLTZ.E.Match (subject, cases))
      (match LLTZ.R.find_leaf cases with
       | Some { lam_var = _; body } -> body.LLTZ.E.type_
       | None -> raise_s [%message "Expected a leaf with lambda"]
      )

  let raw_michelson ~range michelson args return_ty =
    create ~range (LLTZ.E.Raw_michelson { michelson; args }) return_ty

  let create_contract ~range ~storage ~code ~delegate ~initial_balance ~initial_storage =
    create ~range (LLTZ.E.Create_contract {
      storage; code; delegate; initial_balance; initial_storage
    })
      (mk_type ~range (LLTZ.T.Tuple (
         LLTZ.R.Node [
           LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Address);
           LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Operation)
         ]
       )))

  (* Primitives *)
  (* Arity 0 *)
  let amount ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Amount, [])) (mk_type ~range LLTZ.T.Mutez)

  let balance ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Balance, [])) (mk_type ~range LLTZ.T.Mutez)

  let chain_id_prim ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Chain_id, [])) (mk_type ~range LLTZ.T.Chain_id)

  let level ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Level, [])) (mk_type ~range LLTZ.T.Nat)

  let now ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Now, [])) (mk_type ~range LLTZ.T.Timestamp)

  let self ~range str_opt contract_ty =
    create ~range (LLTZ.E.Prim (LLTZ.P.Self str_opt, [])) contract_ty

  let self_address ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Self_address, [])) (mk_type ~range LLTZ.T.Address)

  let sender ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Sender, [])) (mk_type ~range LLTZ.T.Address)

  let source ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Source, [])) (mk_type ~range LLTZ.T.Address)

  let total_voting_power ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Total_voting_power, []))
      (mk_type ~range LLTZ.T.Nat)

  let empty_bigmap ~range key value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Empty_bigmap (key, value), []))
      (mk_type ~range (LLTZ.T.Big_map (key, value)))

  let empty_map ~range key value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Empty_map (key, value), []))
      (mk_type ~range (LLTZ.T.Map (key, value)))

  let empty_set ~range ty =
    create ~range (LLTZ.E.Prim (LLTZ.P.Empty_set ty, []))
      (mk_type ~range (LLTZ.T.Set ty))

  let nil ~range ty =
    create ~range (LLTZ.E.Prim (LLTZ.P.Nil ty, []))
      (mk_type ~range (LLTZ.T.List ty))

  let none ~range ty =
    create ~range (LLTZ.E.Prim (LLTZ.P.None ty, []))
      (mk_type ~range (LLTZ.T.Option ty))

  let sapling_empty_state ~range memo =
    create ~range (LLTZ.E.Prim (LLTZ.P.Sapling_empty_state { memo }, []))
      (mk_type ~range (LLTZ.T.Sapling_state { memo }))

  let unit_prim ~range =
    create ~range (LLTZ.E.Prim (LLTZ.P.Unit, []))
      (mk_type ~range LLTZ.T.Unit)

  (* Arity 1/2 *)
  let car ~range pair =
    create ~range (LLTZ.E.Prim (LLTZ.P.Car, [pair]))
      (match pair.LLTZ.E.type_.LLTZ.T.desc with
       | LLTZ.T.Tuple (LLTZ.R.Node (LLTZ.R.Leaf (_, hd) :: _)) -> hd
       | _ -> raise_s [%message "Expected pair type"]
      )

  let cdr ~range pair =
    create ~range (LLTZ.E.Prim (LLTZ.P.Cdr, [pair]))
      (match pair.LLTZ.E.type_.LLTZ.T.desc with
       | LLTZ.T.Tuple (LLTZ.R.Node (LLTZ.R.Leaf (_,_) :: LLTZ.R.Leaf (_,tl) :: [])) -> tl
       | _ -> raise_s [%message "Expected pair type"]
      )

  let left ~range (opt1, opt2, ty) value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Left (opt1, opt2, ty), [value]))
      (mk_type ~range (LLTZ.T.Or (LLTZ.R.Node [
         LLTZ.R.Leaf (convert_option opt1, value.LLTZ.E.type_);
         LLTZ.R.Leaf (convert_option opt2, ty)
       ])))

  let right ~range (opt1, opt2, ty) value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Right (opt1, opt2, ty), [value]))
      (mk_type ~range (LLTZ.T.Or (LLTZ.R.Node [
         LLTZ.R.Leaf (convert_option opt1, ty);
         LLTZ.R.Leaf (convert_option opt2, value.LLTZ.E.type_)
       ])))

  let some ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Some, [value]))
      (mk_type ~range (LLTZ.T.Option value.LLTZ.E.type_))

  let abs ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Abs, [value])) (mk_type ~range LLTZ.T.Nat)

  let neg ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Neg, [value]))
      (mk_type ~range (
         match value.LLTZ.E.type_.LLTZ.T.desc with
         | LLTZ.T.Int | LLTZ.T.Nat -> LLTZ.T.Int
         | LLTZ.T.Bls12_381_g1
         | LLTZ.T.Bls12_381_g2
         | LLTZ.T.Bls12_381_fr -> value.LLTZ.E.type_.LLTZ.T.desc
         | _ -> raise_s [%message "Expected int,nat or BLS12-381 field/group element type"]
       ))

  let nat_prim ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Nat, [value]))
      (mk_type ~range LLTZ.T.Nat)

  let int_prim ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Int, [value]))
      (mk_type ~range LLTZ.T.Int)

  let bytes_prim ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Bytes, [value]))
      (mk_type ~range LLTZ.T.Bytes)

  let is_nat ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Is_nat, [value]))
      (mk_type ~range (LLTZ.T.Option (mk_type ~range LLTZ.T.Nat)))

  (* comparisons modified to have arity 2 *)
  let compare ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Compare, [lhs; rhs]))
      (mk_type ~range LLTZ.T.Int)

  let eq ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Eq, [compare ~range lhs rhs]))
      (mk_type ~range LLTZ.T.Bool)

  let neq ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Neq, [compare ~range lhs rhs]))
      (mk_type ~range LLTZ.T.Bool)

  let le ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Le, [compare ~range lhs rhs]))
      (mk_type ~range LLTZ.T.Bool)

  let lt ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Lt, [compare ~range lhs rhs]))
      (mk_type ~range LLTZ.T.Bool)

  let ge ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Ge, [compare ~range lhs rhs]))
      (mk_type ~range LLTZ.T.Bool)

  let gt ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Gt, [compare ~range lhs rhs]))
      (mk_type ~range LLTZ.T.Bool)

  let not ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Not, [value]))
      (mk_type ~range (
         match value.LLTZ.E.type_.LLTZ.T.desc with
         | LLTZ.T.Bool -> Bool
         | LLTZ.T.Nat -> Int
         | LLTZ.T.Int -> Int
         | LLTZ.T.Bytes -> Bytes
         | _ -> raise_s [%message "Expected bool, nat, int, or bytes type"]
       ))

  let size ~range container =
    create ~range (LLTZ.E.Prim (LLTZ.P.Size, [container]))
      (mk_type ~range LLTZ.T.Nat)

  let address ~range contract =
    create ~range (LLTZ.E.Prim (LLTZ.P.Address, [contract]))
      (mk_type ~range LLTZ.T.Address)

  let implicit_account ~range key_hash =
    create ~range (LLTZ.E.Prim (LLTZ.P.Implicit_account, [key_hash]))
      (mk_type ~range (LLTZ.T.Contract (mk_type ~range LLTZ.T.Unit)))

  let contract ~range (opt, ty) address =
    create ~range (LLTZ.E.Prim (LLTZ.P.Contract (opt, ty), [address]))
      (mk_type ~range (LLTZ.T.Option (mk_type ~range (LLTZ.T.Contract ty))))

  let pack ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Pack, [value]))
      (mk_type ~range LLTZ.T.Bytes)

  let unpack ~range ty value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Unpack ty, [value]))
      (mk_type ~range (LLTZ.T.Option ty))

  let hash_key ~range key =
    create ~range (LLTZ.E.Prim (LLTZ.P.Hash_key, [key]))
      (mk_type ~range LLTZ.T.Key_hash)

  let blake2b ~range bytes =
    create ~range (LLTZ.E.Prim (LLTZ.P.Blake2b, [bytes]))
      (mk_type ~range LLTZ.T.Bytes)

  let sha256 ~range bytes =
    create ~range (LLTZ.E.Prim (LLTZ.P.Sha256, [bytes]))
      (mk_type ~range LLTZ.T.Bytes)

  let sha512 ~range bytes =
    create ~range (LLTZ.E.Prim (LLTZ.P.Sha512, [bytes]))
      (mk_type ~range LLTZ.T.Bytes)

  let keccak ~range bytes =
    create ~range (LLTZ.E.Prim (LLTZ.P.Keccak, [bytes]))
      (mk_type ~range LLTZ.T.Bytes)

  let sha3 ~range bytes =
    create ~range (LLTZ.E.Prim (LLTZ.P.Sha3, [bytes]))
      (mk_type ~range LLTZ.T.Bytes)

  let set_delegate ~range delegate =
    create ~range (LLTZ.E.Prim (LLTZ.P.Set_delegate, [delegate]))
      (mk_type ~range LLTZ.T.Operation)

  let read_ticket ~range ticket =
    create ~range (LLTZ.E.Prim (LLTZ.P.Read_ticket, [ticket]))
      (mk_type ~range (
         match ticket.LLTZ.E.type_.LLTZ.T.desc with
         | LLTZ.T.Ticket cty ->
           LLTZ.T.Tuple (LLTZ.R.Node [
             LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Address); (* Address part of the ticket *)
             LLTZ.R.Leaf (None, cty); (* The content type of the ticket *)
             LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Nat); (* The amount (Nat) part of the ticket *)
             LLTZ.R.Leaf (None, mk_type ~range (LLTZ.T.Ticket cty)) (* The original ticket type *)
           ])
         | _ -> raise_s [%message "Expected ticket type"]
       ))

  let join_tickets ~range ticket1 ticket2 =
    create ~range (LLTZ.E.Prim (LLTZ.P.Join_tickets, [ticket1; ticket2]))
      (mk_type ~range (
         match (ticket1.LLTZ.E.type_.LLTZ.T.desc, ticket2.LLTZ.E.type_.LLTZ.T.desc) with
         | LLTZ.T.Ticket cty1, LLTZ.T.Ticket _ ->
           LLTZ.T.Option (mk_type ~range (LLTZ.T.Ticket cty1))
         | _ -> raise_s [%message "Expected two tickets of the same type"]
       ))

  let pairing_check ~range pairings =
    create ~range (LLTZ.E.Prim (LLTZ.P.Pairing_check, [pairings]))
      (mk_type ~range LLTZ.T.Bool)

  let voting_power ~range key_hash =
    create ~range (LLTZ.E.Prim (LLTZ.P.Voting_power, [key_hash]))
      (mk_type ~range LLTZ.T.Nat)

  let getn ~range n _value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Get_n n, [_value])) (assert false) (*TODO*)

  let cast ~range ty value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Cast ty, [value])) ty

  let emit ~range (opt, ty) value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Emit (opt, ty), [value]))
      (mk_type ~range LLTZ.T.Operation)

  let failwith ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Failwith, [value]))
      (mk_type ~range LLTZ.T.Unit)

  let never ~range value =
    create ~range (LLTZ.E.Prim (LLTZ.P.Never, [value]))
      (mk_type ~range LLTZ.T.Unit)

  let pair ~range (opt1, opt2) first second =
    create ~range (LLTZ.E.Prim (LLTZ.P.Pair (opt1, opt2), [first; second]))
      (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [
         LLTZ.R.Leaf (convert_option opt1, first.LLTZ.E.type_);
         LLTZ.R.Leaf (convert_option opt2, second.LLTZ.E.type_)
       ])))

  let add ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Add, [lhs; rhs]))
      (let open LLTZ.T in
       mk_type ~range (
         match (lhs.LLTZ.E.type_.desc, rhs.LLTZ.E.type_.desc) with
         | Nat, Nat -> Nat
         | Nat, Int -> Int
         | Int, Nat -> Int
         | Int, Int -> Int
         | Timestamp, Int -> Timestamp
         | Int, Timestamp -> Timestamp
         | Mutez, Mutez -> Mutez
         | Bls12_381_g1, Bls12_381_g1 -> Bls12_381_g1
         | Bls12_381_g2, Bls12_381_g2 -> Bls12_381_g2
         | Bls12_381_fr, Bls12_381_fr -> Bls12_381_fr
         | _ -> raise_s [%message "Expected matching types for ADD operation"]
       ))

  let mul ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Mul, [lhs; rhs]))
      (let open LLTZ.T in
       mk_type ~range (
         match (lhs.LLTZ.E.type_.desc, rhs.LLTZ.E.type_.desc) with
         | Nat, Nat -> Nat
         | Nat, Int -> Int
         | Int, Nat -> Int
         | Int, Int -> Int
         | Mutez, Nat -> Mutez
         | Nat, Mutez -> Mutez
         | Bls12_381_g1, Bls12_381_fr -> Bls12_381_g1
         | Bls12_381_g2, Bls12_381_fr -> Bls12_381_g2
         | Bls12_381_fr, Bls12_381_fr -> Bls12_381_fr
         | Nat, Bls12_381_fr -> Bls12_381_fr
         | Int, Bls12_381_fr -> Bls12_381_fr
         | Bls12_381_fr, Nat -> Bls12_381_fr
         | Bls12_381_fr, Int -> Bls12_381_fr
         | _ -> raise_s [%message "Expected matching types for MUL operation"]
       ))

  let sub ~range lhs rhs =
    match (lhs.LLTZ.E.type_.desc, rhs.LLTZ.E.type_.desc) with
    | LLTZ.T.Mutez, LLTZ.T.Mutez ->
      create ~range (LLTZ.E.Prim (LLTZ.P.Sub_mutez, [lhs; rhs]))
        (mk_type ~range (LLTZ.T.Option (mk_type ~range LLTZ.T.Mutez)))
    | _ ->
      create ~range (LLTZ.E.Prim (LLTZ.P.Sub, [lhs; rhs]))
        (let open LLTZ.T in
         mk_type ~range (
           match (lhs.LLTZ.E.type_.desc, rhs.LLTZ.E.type_.desc) with
           | Nat, Nat -> Int
           | Nat, Int -> Int
           | Int, Nat -> Int
           | Int, Int -> Int
           | Timestamp, Int -> Timestamp
           | Timestamp, Timestamp -> Int
           | _ -> raise_s [%message "Expected matching types for SUB operation"]
         ))

  let sub_mutez ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Sub_mutez, [lhs; rhs]))
      (mk_type ~range (LLTZ.T.Option (mk_type ~range LLTZ.T.Mutez)))

  let lsr_ ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Lsr, [lhs; rhs]))
      lhs.LLTZ.E.type_

  let lsl_ ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Lsl, [lhs; rhs]))
      lhs.LLTZ.E.type_

  let xor ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Xor, [lhs; rhs]))
      lhs.LLTZ.E.type_

  let ediv ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Ediv, [lhs; rhs]))
      (let open LLTZ.T in
       mk_type ~range (
         match (lhs.LLTZ.E.type_.desc, rhs.LLTZ.E.type_.desc) with
         | Nat, Nat ->
           Option (mk_type ~range (Tuple (LLTZ.R.Node [
             LLTZ.R.Leaf (None, mk_type ~range Nat);
             LLTZ.R.Leaf (None, mk_type ~range Nat)
           ])))
         | Nat, Int
         | Int, Nat
         | Int, Int ->
           Option (mk_type ~range (Tuple (LLTZ.R.Node [
             LLTZ.R.Leaf (None, mk_type ~range Int);
             LLTZ.R.Leaf (None, mk_type ~range Nat)
           ])))
         | Mutez, Nat ->
           Option (mk_type ~range (Tuple (LLTZ.R.Node [
             LLTZ.R.Leaf (None, mk_type ~range Mutez);
             LLTZ.R.Leaf (None, mk_type ~range Mutez)
           ])))
         | Mutez, Mutez ->
           Option (mk_type ~range (Tuple (LLTZ.R.Node [
             LLTZ.R.Leaf (None, mk_type ~range Nat);
             LLTZ.R.Leaf (None, mk_type ~range Mutez)
           ])))
         | _ -> raise_s [%message "Expected matching types for EDIV operation"]
       ))

  let div_ ~range (lhs:Expr.t) (rhs:Expr.t) =
    (if_none ~range (ediv ~range lhs rhs) ~some:(
    let var_ty = tuple_ty ~range (Row.Node [Row.Leaf (None, lhs.type_); Row.Leaf(None, rhs.type_)]) in
    let var_name = Name.create () in
    {lam_var=(Var var_name, var_ty ); 
    body = car ~range (variable ~range (Var var_name) (var_ty ))})
    ~none:(failwith ~range (string ~range "DIV by 0")))
  
  let mod_ ~range (lhs:Expr.t) (rhs:Expr.t) =
    (if_none ~range (ediv ~range lhs rhs) ~some:(
    let var_ty = tuple_ty ~range (Row.Node [Row.Leaf (None, lhs.type_); Row.Leaf(None, rhs.type_)]) in
    let var_name = Name.create () in
    {lam_var=(Var var_name, var_ty ); 
    body = cdr ~range (variable ~range (Var var_name) (var_ty ))})
    ~none:(failwith ~range (string ~range "MOD by 0")))

  let and_ ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.And, [lhs; rhs]))
      rhs.LLTZ.E.type_

  let or_ ~range lhs rhs =
    create ~range (LLTZ.E.Prim (LLTZ.P.Or, [lhs; rhs]))
      lhs.LLTZ.E.type_

  let cons ~range head tail =
    create ~range (LLTZ.E.Prim (LLTZ.P.Cons, [head; tail]))
      tail.LLTZ.E.type_

  let concat1 ~range val1 str2 =
    create ~range (LLTZ.E.Prim (LLTZ.P.Concat1, [val1; str2]))
      (mk_type ~range LLTZ.T.String)

  let concat2 ~range val1 bytes2 =
    create ~range (LLTZ.E.Prim (LLTZ.P.Concat2, [val1; bytes2]))
      (mk_type ~range LLTZ.T.Bytes)

  let get ~range key collection =
    create ~range (LLTZ.E.Prim (LLTZ.P.Get, [key; collection]))
      (mk_type ~range (
         match collection.LLTZ.E.type_.LLTZ.T.desc with
         | LLTZ.T.Map (_, vty)
         | LLTZ.T.Big_map (_, vty) -> LLTZ.T.Option vty
         | _ -> raise_s [%message "Expected a map or big_map with matching key type"]
       ))

  let mem ~range key collection =
    create ~range (LLTZ.E.Prim (LLTZ.P.Mem, [key; collection]))
      (mk_type ~range LLTZ.T.Bool)

  let exec ~range value lambda =
    create ~range (LLTZ.E.Prim (LLTZ.P.Exec, [value; lambda]))
      (match lambda.LLTZ.E.type_.LLTZ.T.desc with
       | LLTZ.T.Function (_, ret_ty) -> ret_ty
       | _ -> raise_s [%message "Expected function type"]
      )

  let apply ~range value lambda =
    create ~range (LLTZ.E.Prim (LLTZ.P.Apply, [value; lambda]))
      (match lambda.LLTZ.E.type_.LLTZ.T.desc with
       | LLTZ.T.Function ({ LLTZ.T.desc = LLTZ.T.Tuple (
            LLTZ.R.Node [
              LLTZ.R.Leaf (None, _);
              LLTZ.R.Leaf (None, ty2)
            ]
          ); _ }, ty3) ->
         mk_type ~range (LLTZ.T.Function (ty2, ty3))
       | _ -> raise_s [%message "Expected function type"]
      )

  let sapling_verify_update ~range transaction state =
    create ~range (LLTZ.E.Prim (LLTZ.P.Sapling_verify_update, [transaction; state]))
      (let open LLTZ.T in
       match (transaction.LLTZ.E.type_.desc, state.LLTZ.E.type_.desc) with
       | Sapling_transaction _, Sapling_state ms2 ->
         mk_type ~range (Option (
           mk_type ~range (Tuple (LLTZ.R.Node [
             LLTZ.R.Leaf (None, mk_type ~range Bytes);
             LLTZ.R.Leaf (None, mk_type ~range Int);
             LLTZ.R.Leaf (None, mk_type ~range (Sapling_state ms2))
           ]))
         ))
       | _ -> raise_s [%message "Expected matching sapling_transaction and sapling_state types"]
      )

  let ticket ~range content amount =
    create ~range (LLTZ.E.Prim (LLTZ.P.Ticket, [content; amount]))
      (mk_type ~range (
         LLTZ.T.Option (mk_type ~range (LLTZ.T.Ticket content.LLTZ.E.type_))
       ))

  let ticket_deprecated ~range content amount =
    create ~range (LLTZ.E.Prim (LLTZ.P.Ticket_deprecated, [content; amount]))
      (mk_type ~range (
         LLTZ.T.Option (mk_type ~range (LLTZ.T.Ticket content.LLTZ.E.type_))
       ))

  let split_ticket ~range ticket amounts =
    create ~range (LLTZ.E.Prim (LLTZ.P.Split_ticket, [ticket; amounts]))
      (mk_type ~range (
         LLTZ.T.Option (
           mk_type ~range (LLTZ.T.Tuple (
               LLTZ.R.Node [
                 LLTZ.R.Leaf (None, mk_type ~range (LLTZ.T.Ticket ticket.LLTZ.E.type_));
                 LLTZ.R.Leaf (None, mk_type ~range (LLTZ.T.Ticket ticket.LLTZ.E.type_))
               ]
             ))
         )
       ))

  let updaten ~range n value pair =
    create ~range (LLTZ.E.Prim (LLTZ.P.Update_n n, [value; pair])) (assert false) (* TODO *)

  let view ~range name ~return_type ~d ~address =
    create ~range (LLTZ.E.Prim (LLTZ.P.View (name, return_type), [d; address]))
      (mk_type ~range (LLTZ.T.Option return_type))

  (* Arity 3 *)
  let slice ~range offset ~length ~seq =
    create ~range (LLTZ.E.Prim (LLTZ.P.Slice, [offset; length; seq]))
      (mk_type ~range (LLTZ.T.Option seq.LLTZ.E.type_))

  let update ~range key value ~of_ =
    create ~range (LLTZ.E.Prim (LLTZ.P.Update, [key; value; of_])) of_.LLTZ.E.type_

  let get_and_update ~range key value ~of_ =
    create ~range (LLTZ.E.Prim (LLTZ.P.Get_and_update, [key; value; of_]))
      (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [
         LLTZ.R.Leaf (None, value.LLTZ.E.type_);
         LLTZ.R.Leaf (None, of_.LLTZ.E.type_)
       ])))

  let transfer_tokens ~range param ~amount ~contract =
    create ~range (LLTZ.E.Prim (LLTZ.P.Transfer_tokens, [param; amount; contract]))
      (mk_type ~range LLTZ.T.Operation)

  let check_signature ~range key ~signature ~message =
    create ~range (LLTZ.E.Prim (LLTZ.P.Check_signature, [key; signature; message]))
      (mk_type ~range LLTZ.T.Bool)

  let open_chest ~range chest_key ~chest ~time =
    create ~range (LLTZ.E.Prim (LLTZ.P.Open_chest, [chest_key; chest; time]))
      (mk_type ~range (LLTZ.T.Option (mk_type ~range LLTZ.T.Bytes)))

  let convert_list (exprs: LLTZ.E.t list) : LLTZ.E.t LLTZ.R.t =
    let converted_row_leaves =
      List.map ~f:(fun expr -> LLTZ.R.Leaf (None, expr)) exprs
    in
    LLTZ.R.Node converted_row_leaves

  let gen_name () = Name.create ()

  let annon_function var_name var_ty ~body : LLTZ.E.lambda =
    { lam_var = (Var var_name, var_ty); body }

  let global_constant ~range hash args val_ty =
    create ~range (LLTZ.E.Global_constant {hash; args}) val_ty
end

module With_dummy = struct
  open Dummy_range

  let unit =
    Default.unit ~range:v

  let bool b =
    Default.bool ~range:v b

  let nat n =
    Default.nat ~range:v n

  let int n =
    Default.int ~range:v n

  let mutez n =
    Default.mutez ~range:v n

  let string s =
    Default.string ~range:v s

  let key s =
    Default.key ~range:v s

  let key_hash s =
    Default.key_hash ~range:v s

  let bytes s =
    Default.bytes ~range:v s

  let chain_id s =
    Default.chain_id ~range:v s

  let address_const s =
    Default.address_const ~range:v s

  let timestamp s =
    Default.timestamp ~range:v s

  let bls12_381_g1 s =
    Default.bls12_381_g1 ~range:v s

  let bls12_381_g2 s =
    Default.bls12_381_g2 ~range:v s

  let bls12_381_fr s =
    Default.bls12_381_fr ~range:v s

  let signature s =
    Default.signature ~range:v s

  let var = Default.var
  let mut_var = Default.mut_var

  let unit_ty = Default.unit_ty ~range:v
  let bool_ty = Default.bool_ty ~range:v
  let nat_ty = Default.nat_ty ~range:v
  let int_ty = Default.int_ty ~range:v
  let mutez_ty = Default.mutez_ty ~range:v
  let string_ty = Default.string_ty ~range:v
  let bytes_ty = Default.bytes_ty ~range:v
  let chain_id_ty = Default.chain_id_ty ~range:v
  let timestamp_ty = Default.timestamp_ty ~range:v
  let address_ty = Default.address_ty ~range:v
  let key_ty = Default.key_ty ~range:v
  let key_hash_ty = Default.key_hash_ty ~range:v
  let signature_ty = Default.signature_ty ~range:v
  let operation_ty = Default.operation_ty ~range:v
  let sapling_state_ty memo_size = Default.sapling_state_ty ~range:v memo_size
  let sapling_transaction_ty memo_size = Default.sapling_transaction_ty ~range:v memo_size
  let bls12_381_g1_ty = Default.bls12_381_g1_ty ~range:v
  let bls12_381_g2_ty = Default.bls12_381_g2_ty ~range:v
  let bls12_381_fr_ty = Default.bls12_381_fr_ty ~range:v
  let tuple_ty row = Default.tuple_ty ~range:v row
  let or_ty row = Default.or_ty ~range:v row
  let option_ty ty = Default.option_ty ~range:v ty
  let list_ty ty = Default.list_ty ~range:v ty
  let set_ty ty = Default.set_ty ~range:v ty
  let contract_ty ty = Default.contract_ty ~range:v ty
  let ticket_ty ty = Default.ticket_ty ~range:v ty
  let function_ty arg ret = Default.function_ty ~range:v arg ret
  let map_ty key value = Default.map_ty ~range:v key value
  let big_map_ty key value = Default.big_map_ty ~range:v key value
  let mk_row = Default.mk_row
  let mk_tuple_ty list = Default.mk_tuple_ty ~range:v list

  let variable var var_ty = Default.variable ~range:v var var_ty
  let let_in var ~rhs ~in_ = Default.let_in ~range:v var ~rhs ~in_
  let ( let* ) = Default.( let* )
  let lambda (var,var_ty) ~body = Default.lambda ~range:v (var,var_ty) ~body
  let lambda_rec mu_var (var, var_ty) ~body =
    Default.lambda_rec ~range:v mu_var (var, var_ty) ~body
  let app abs arg = Default.app ~range:v abs arg
  let let_mut_in var ~rhs ~in_ = Default.let_mut_in ~range:v var ~rhs ~in_
  let deref var var_ty = Default.deref ~range:v var var_ty
  let assign var value = Default.assign ~range:v var value
  let if_bool condition ~then_ ~else_ = Default.if_bool ~range:v condition ~then_ ~else_
  let if_none subject ~none ~some = Default.if_none ~range:v subject ~none ~some
  let if_cons subject ~empty ~nonempty = Default.if_cons ~range:v subject ~empty ~nonempty
  let if_left subject ~left ~right = Default.if_left ~range:v subject ~left ~right
  let while_ cond ~body = Default.while_ ~range:v cond ~body
  let while_left cond ~body = Default.while_left ~range:v cond ~body
  let for_ index ~init ~cond ~update ~body = Default.for_ ~range:v index ~init ~cond ~update ~body
  let for_each collection ~body = Default.for_each ~range:v collection ~body
  let map collection ~map = Default.map ~range:v collection ~map
  let fold_left collection ~init ~fold = Default.fold_left ~range:v collection ~init ~fold
  let fold_right collection ~init ~fold = Default.fold_right ~range:v collection ~init ~fold
  let let_tuple_in components ~rhs ~in_ = Default.let_tuple_in ~range:v components ~rhs ~in_
  let tuple row = Default.tuple ~range:v row
  let proj tuple ~path = Default.proj ~range:v tuple ~path
  let update_tuple tuple ~component ~update = Default.update_tuple ~range:v tuple ~component ~update
  let inj context expr = Default.inj ~range:v context expr
  let match_ subject ~cases = Default.match_ ~range:v subject ~cases
  let raw_michelson michelson args return_ty = Default.raw_michelson ~range:v michelson args return_ty
  let create_contract ~storage ~code ~delegate ~initial_balance ~initial_storage =
    Default.create_contract ~range:v ~storage ~code ~delegate ~initial_balance ~initial_storage
  let amount = Default.amount ~range:v
  let balance = Default.balance ~range:v
  let chain_id_prim = Default.chain_id_prim ~range:v
  let level = Default.level ~range:v
  let now = Default.now ~range:v
  let self str_opt contract_ty = Default.self ~range:v str_opt contract_ty
  let self_address = Default.self_address ~range:v
  let sender = Default.sender ~range:v
  let source = Default.source ~range:v
  let total_voting_power = Default.total_voting_power ~range:v
  let empty_bigmap key value = Default.empty_bigmap ~range:v key value
  let empty_map key value = Default.empty_map ~range:v key value
  let empty_set ty = Default.empty_set ~range:v ty
  let nil ty = Default.nil ~range:v ty
  let none ty = Default.none ~range:v ty
  let sapling_empty_state memo = Default.sapling_empty_state ~range:v memo
  let unit_prim = Default.unit_prim ~range:v
  let car pair = Default.car ~range:v pair
  let cdr pair = Default.cdr ~range:v pair
  let left (opt1, opt2, ty) value = Default.left ~range:v (opt1, opt2, ty) value
  let right (opt1, opt2, ty) value = Default.right ~range:v (opt1, opt2, ty) value
  let some value = Default.some ~range:v value
  let abs value = Default.abs ~range:v value
  let neg value = Default.neg ~range:v value
  let nat_prim value = Default.nat_prim ~range:v value
  let int_prim value = Default.int_prim ~range:v value
  let bytes_prim value = Default.bytes_prim ~range:v value
  let is_nat value = Default.is_nat ~range:v value
  let compare lhs rhs = Default.compare ~range:v lhs rhs
  let eq lhs rhs = Default.eq ~range:v lhs rhs
  let neq lhs rhs = Default.neq ~range:v lhs rhs
  let le lhs rhs = Default.le ~range:v lhs rhs
  let lt lhs rhs = Default.lt ~range:v lhs rhs
  let ge lhs rhs = Default.ge ~range:v lhs rhs
  let gt lhs rhs = Default.gt ~range:v lhs rhs
  let not value = Default.not ~range:v value
  let size container = Default.size ~range:v container
  let address contract = Default.address ~range:v contract
  let implicit_account key_hash = Default.implicit_account ~range:v key_hash
  let contract (opt, ty) address = Default.contract ~range:v (opt, ty) address
  let pack value = Default.pack ~range:v value
  let unpack ty value = Default.unpack ~range:v ty value
  let hash_key key = Default.hash_key ~range:v key
  let blake2b bytes = Default.blake2b ~range:v bytes
  let sha256 bytes = Default.sha256 ~range:v bytes
  let sha512 bytes = Default.sha512 ~range:v bytes
  let keccak bytes = Default.keccak ~range:v bytes
  let sha3 bytes = Default.sha3 ~range:v bytes
  let set_delegate delegate = Default.set_delegate ~range:v delegate
  let read_ticket ticket = Default.read_ticket ~range:v ticket
  let join_tickets ticket1 ticket2 = Default.join_tickets ~range:v ticket1 ticket2
  let pairing_check pairings = Default.pairing_check ~range:v pairings
  let voting_power key_hash = Default.voting_power ~range:v key_hash
  let getn n value = Default.getn ~range:v n value
  let cast ty value = Default.cast ~range:v ty value
  let emit (opt, ty) value = Default.emit ~range:v (opt, ty) value
  let failwith value = Default.failwith ~range:v value
  let never value = Default.never ~range:v value
  let pair (opt1, opt2) first second = Default.pair ~range:v (opt1, opt2) first second
  let add lhs rhs = Default.add ~range:v lhs rhs
  let mul lhs rhs = Default.mul ~range:v lhs rhs
  let sub lhs rhs = Default.sub ~range:v lhs rhs
  let sub_mutez lhs rhs = Default.sub_mutez ~range:v lhs rhs
  let lsr_ lhs rhs = Default.lsr_ ~range:v lhs rhs
  let lsl_ lhs rhs = Default.lsl_ ~range:v lhs rhs
  let xor lhs rhs = Default.xor ~range:v lhs rhs
  let ediv lhs rhs = Default.ediv ~range:v lhs rhs
  let div_ lhs rhs = Default.div_ ~range:v lhs rhs
  let mod_ lhs rhs = Default.mod_ ~range:v lhs rhs
  let and_ lhs rhs = Default.and_ ~range:v lhs rhs
  let or_ lhs rhs = Default.or_ ~range:v lhs rhs
  let cons head tail = Default.cons ~range:v head tail
  let concat1 val1 str2 = Default.concat1 ~range:v val1 str2
  let concat2 val1 bytes2 = Default.concat2 ~range:v val1 bytes2
  let get key collection = Default.get ~range:v key collection
  let mem key collection = Default.mem ~range:v key collection
  let exec value lambda = Default.exec ~range:v value lambda
  let apply value lambda = Default.apply ~range:v value lambda
  let sapling_verify_update transaction state =
    Default.sapling_verify_update ~range:v transaction state
  let ticket content amount = Default.ticket ~range:v content amount
  let ticket_deprecated content amount =
    Default.ticket_deprecated ~range:v content amount
  let split_ticket ticket amounts = Default.split_ticket ~range:v ticket amounts
  let updaten n value pair = Default.updaten ~range:v n value pair
  let view name ~return_type ~d ~address =
    Default.view ~range:v name ~return_type ~d ~address
  let slice offset ~length ~seq = Default.slice ~range:v offset ~length ~seq
  let update key value ~of_ = Default.update ~range:v key value ~of_
  let get_and_update key value ~of_ = Default.get_and_update ~range:v key value ~of_
  let transfer_tokens param ~amount ~contract =
    Default.transfer_tokens ~range:v param ~amount ~contract
  let check_signature key ~signature ~message =
    Default.check_signature ~range:v key ~signature ~message
  let open_chest chest_key ~chest ~time =
    Default.open_chest ~range:v chest_key ~chest ~time
  let convert_list exprs = Default.convert_list exprs
  let gen_name = Default.gen_name
  let annon_function var_name var_ty ~body = Default.annon_function var_name var_ty ~body
  let global_constant hash args val_ty = Default.global_constant ~range:v hash args val_ty
end

include Default
