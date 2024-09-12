(* dsl.ml 
   This file adds a domain specific language (DSL), consisting of functions for easier creation of expressions using LLTZ-IR. 
*)
open Grace
open Core 

module LLTZ = struct
  module E = Expr
  module T = Type
  module P = Primitive
  module R = Row
end

type var = Var of string
type mut_var = Mut_var of string

let dummy : Range.t = Range.initial (`String { content = ""; name = Some "" })

(* Creation with optional range *)
let create ?(range = dummy) desc type_ = LLTZ.E.{ desc; range; type_} 
let mk_type ?(range=dummy) (desc: LLTZ.T.desc) : LLTZ.T.t = { desc; range}

let rec get_type_row (row: LLTZ.E.t Row.t) : LLTZ.T.t Row.t =
  match row with
  | Row.Leaf (label_opt, expr) -> Row.Leaf (label_opt, expr.type_)
  | Row.Node exprs -> Row.Node (List.map ~f:(fun x -> get_type_row x) exprs)

let rec get_proj_type (row: LLTZ.T.t Row.t) (path: int list) : LLTZ.T.t =
  match path with
  | i :: path ->
    (match row with
    | Row.Leaf (_, _) -> raise_s [%message "Invalid path" (path : int list) (row : LLTZ.T.t Row.t)]
    | Row.Node row_list ->
      (match List.nth row_list i with
     | Some expr -> get_proj_type expr path
     | None -> raise_s [%message "Invalid path" (path : int list) (row : LLTZ.T.t Row.t)]))
  | [] -> (match row with
    | Row.Leaf (_, type_) -> type_
    | Row.Node _ -> raise_s [%message "Invalid path" (path : int list) (row : LLTZ.T.t Row.t)])

let get_inj_type (context: Type.t LLTZ.R.Context.t) : LLTZ.T.desc =
  match context with
  | LLTZ.R.Context.Hole ty -> ty.desc
  | LLTZ.R.Context.Node (left_val, mid, right_val) ->
    let full = left_val @ [ mid ] @ right_val in
    LLTZ.T.Or (Node full)

let convert_option str_opt =
  Option.map ~f:(fun str -> LLTZ.R.Label str) str_opt

(* Constants *)
let unit ?(range = dummy) () = create ~range (LLTZ.E.Const(Unit)) (mk_type ~range LLTZ.T.Unit)
let bool ?(range = dummy) b = create ~range (LLTZ.E.Const(Bool b)) (mk_type ~range LLTZ.T.Bool)
let nat ?(range = dummy) n = create ~range (LLTZ.E.Const(Nat (Z.of_int n))) (mk_type ~range LLTZ.T.Nat)
let int ?(range = dummy) n = create ~range (LLTZ.E.Const(Int (Z.of_int n))) (mk_type ~range LLTZ.T.Int)
let mutez ?(range = dummy) n = create ~range (LLTZ.E.Const(Mutez (Z.of_int n))) (mk_type ~range LLTZ.T.Mutez)
let string ?(range = dummy) s = create ~range (LLTZ.E.Const(String s)) (mk_type ~range LLTZ.T.String)
let key ?(range = dummy) s = create ~range (LLTZ.E.Const(Key s)) (mk_type ~range LLTZ.T.Keys)
let key_hash ?(range = dummy) s = create ~range (LLTZ.E.Const(Key_hash s)) (mk_type ~range LLTZ.T.Key_hash)
let bytes ?(range = dummy) s = create ~range (LLTZ.E.Const(Bytes s)) (mk_type ~range LLTZ.T.Bytes)
let chain_id ?(range = dummy) s = create ~range (LLTZ.E.Const(Chain_id s)) (mk_type ~range LLTZ.T.Chain_id)
let address_const ?(range = dummy) s = create ~range (LLTZ.E.Const(Address s)) (mk_type ~range LLTZ.T.Address)
let timestamp ?(range = dummy) s = create ~range (LLTZ.E.Const(Timestamp s)) (mk_type ~range LLTZ.T.Timestamp)
let bls12_381_g1 ?(range = dummy) s = create ~range (LLTZ.E.Const(Bls12_381_g1 s)) (mk_type ~range LLTZ.T.Bls12_381_g1)
let bls12_381_g2 ?(range = dummy) s = create ~range (LLTZ.E.Const(Bls12_381_g2 s)) (mk_type ~range LLTZ.T.Bls12_381_g2)
let bls12_381_fr ?(range = dummy) s = create ~range (LLTZ.E.Const(Bls12_381_fr s)) (mk_type ~range LLTZ.T.Bls12_381_fr)
let signature ?(range = dummy) s = create ~range (LLTZ.E.Const(Signature s)) (mk_type ~range LLTZ.T.Signature)

(* Variables *)
let var name = LLTZ.E.Var name
let mut_var name = LLTZ.E.Mut_var name

(* Types *)
let unit_ty ?(range = dummy) () = LLTZ.T.{ desc = Unit; range }
let bool_ty ?(range = dummy) () = LLTZ.T.{ desc = Bool; range }
let nat_ty ?(range = dummy) () = LLTZ.T.{ desc = Nat; range }
let int_ty ?(range = dummy) () = LLTZ.T.{ desc = Int; range }
let mutez_ty ?(range = dummy) () = LLTZ.T.{ desc = Mutez; range }
let string_ty ?(range = dummy) () = LLTZ.T.{ desc = String; range }
let bytes_ty ?(range = dummy) () = LLTZ.T.{ desc = Bytes; range }
let chain_id_ty ?(range = dummy) () = LLTZ.T.{ desc = Chain_id; range }
let timestamp_ty ?(range = dummy) () = LLTZ.T.{ desc = Timestamp; range }
let address_ty ?(range = dummy) () = LLTZ.T.{ desc = Address; range }
let key_ty ?(range = dummy) () = LLTZ.T.{ desc = Keys; range }
let key_hash_ty ?(range = dummy) () = LLTZ.T.{ desc = Key_hash; range }
let signature_ty ?(range = dummy) () = LLTZ.T.{ desc = Signature; range }

let tuple_ty ?(range = dummy) row = LLTZ.T.{ desc = Tuple row; range }
let or_ty ?(range = dummy) row = LLTZ.T.{ desc = Or row; range }
let option_ty ?(range = dummy) ty = LLTZ.T.{ desc = Option ty; range }
let list_ty ?(range = dummy) ty = LLTZ.T.{ desc = List ty; range }
let set_ty ?(range = dummy) ty = LLTZ.T.{ desc = Set ty; range }
let contract_ty ?(range = dummy) ty = LLTZ.T.{ desc = Contract ty; range }
let ticket_ty ?(range = dummy) ty = LLTZ.T.{ desc = Ticket ty; range }
let function_ty ?(range = dummy) arg ret = LLTZ.T.{ desc = Function (arg, ret); range }
let map_ty ?(range = dummy) key value = LLTZ.T.{ desc = Map (key, value); range }
let big_map_ty ?(range = dummy) key value = LLTZ.T.{ desc = Big_map (key, value); range }

let mk_row row = LLTZ.R.Node row

let mk_tuple_ty ?(range = dummy) list = tuple_ty ~range (mk_row (List.map ~f:(fun x -> LLTZ.R.Leaf (None, x)) list))

(* Primitives *)

(* Expressions *)
let variable ?(range = dummy) var var_ty = create ~range (LLTZ.E.Variable var) var_ty
let let_in ?(range = dummy) var ~rhs ~in_ = create ~range (LLTZ.E.Let_in { let_var = var; rhs; in_ }) in_.type_
let ( let* ) rhs in_ = let xname = Name.create () in let_in (Var xname) ~rhs ~in_ 
let lambda ?(range = dummy) (var,var_ty) ~body = create ~range (LLTZ.E.Lambda { lam_var = (var,var_ty); body }) (mk_type (LLTZ.T.Function (var_ty, body.type_)))

let lambda_rec ?(range = dummy) (mu_var) (var, var_ty) ~(body:LLTZ.E.t) = create ~range (Lambda_rec { mu_var = (mu_var, mk_type (LLTZ.T.Function (var_ty, body.type_)) ~range); lambda = { lam_var = (var, var_ty); body = body } }) (mk_type (LLTZ.T.Function (var_ty, body.type_)))
let app ?(range = dummy) (abs:LLTZ.E.t) arg = 
  let ret_ty =
    match abs.type_.desc with
    | LLTZ.T.Function (_, ret_ty) -> ret_ty
    | _ -> raise_s [%message "Expected function type" (abs.type_ : LLTZ.T.t)]
  in
  create ~range (LLTZ.E.App { abs; arg }) ret_ty
let let_mut_in ?(range = dummy) var ~rhs ~in_ = create ~range (LLTZ.E.Let_mut_in { let_var = var; rhs; in_ }) in_.type_
let deref ?(range = dummy) var var_ty = create ~range (LLTZ.E.Deref var) var_ty
let assign ?(range = dummy) var value = create ~range (LLTZ.E.Assign (var, value)) (unit_ty ())
let if_bool ?(range = dummy) condition ~then_ ~else_ = create ~range (LLTZ.E.If_bool { condition; if_true = then_; if_false = else_ }) then_.type_
let if_none ?(range = dummy) subject ~none ~some = create ~range (LLTZ.E.If_none { subject; if_none = none; if_some = some }) none.type_
let if_cons ?(range = dummy) subject ~empty ~nonempty = create ~range (LLTZ.E.If_cons { subject; if_empty = empty; if_nonempty = nonempty }) empty.type_
let if_left ?(range = dummy) subject ~left ~right = create ~range (LLTZ.E.If_left { subject; if_left = left; if_right = right }) left.body.type_
let while_ ?(range = dummy) cond ~body = create ~range (LLTZ.E.While { cond; body }) (unit_ty ())
let while_left ?(range = dummy) cond ~body = create ~range (LLTZ.E.While_left { cond; body }) (
  match body.body.type_.desc with
  | LLTZ.T.Or (LLTZ.R.Node [LLTZ.R.Leaf (_, left_ty); LLTZ.R.Leaf (_, right_ty)]) -> right_ty
  | _ -> raise_s [%message "Expected or type" (body.body.type_ : LLTZ.T.t)]
)
let for_ ?(range = dummy) index ~init ~cond ~update ~body = create ~range (LLTZ.E.For { index; init; cond; update; body }) body.type_
let for_each ?(range = dummy) collection ~body = create ~range (LLTZ.E.For_each { collection; body }) (unit_ty ())
let map ?(range = dummy) collection ~map = create ~range (LLTZ.E.Map { collection; map }) (
  match collection.type_.desc with
    | LLTZ.T.List ty1 -> mk_type (LLTZ.T.List map.body.type_) ~range
    | LLTZ.T.Option ty1 -> mk_type (LLTZ.T.Option map.body.type_) ~range
    | LLTZ.T.Map (kty, ty1) -> mk_type (LLTZ.T.Map (kty, map.body.type_)) ~range
    | _ -> raise_s [%message "Expected list, option, or map type" (collection.type_ : LLTZ.T.t)]
)
let fold_left ?(range = dummy) collection ~init ~fold = create ~range (LLTZ.E.Fold_left { collection; init; fold }) fold.body.type_
let fold_right ?(range = dummy) collection ~init ~fold = create ~range (LLTZ.E.Fold_right { collection; init; fold }) fold.body.type_
let let_tuple_in ?(range = dummy) components ~rhs ~in_ = create ~range (LLTZ.E.Let_tuple_in { components; rhs; in_ }) in_.type_
let tuple ?(range = dummy) row = create ~range (LLTZ.E.Tuple row) (mk_type (LLTZ.T.Tuple (get_type_row row)) ~range)
let proj ?(range = dummy) tuple ~path = create ~range (LLTZ.E.Proj (tuple, path)) (
  match tuple, path with 
  | LLTZ.E.{ type_ = { desc = Tuple tuple_row; _ }; _ }, Here path_list -> get_proj_type tuple_row path_list
  | _ -> raise_s [%message "Expected tuple type" (tuple : LLTZ.E.t)])
let update_tuple ?(range = dummy) tuple ~component ~update = create ~range (LLTZ.E.Update { tuple; component; update }) tuple.type_
let inj ?(range = dummy) context expr = create ~range (LLTZ.E.Inj (context, expr)) (mk_type (get_inj_type context) ~range)
let match_ ?(range = dummy) subject ~cases = create ~range (LLTZ.E.Match (subject, cases)) (
  match LLTZ.R.find_leaf cases with
    | Some leaf -> (
      match leaf with
      | {lam_var = _; body} -> body.type_
    )
  | None -> raise_s [%message "Expected a leaf with lambda" (cases : LLTZ.E.lambda LLTZ.R.t)]
  )
let raw_michelson ?(range = dummy) michelson args return_ty = create ~range (LLTZ.E.Raw_michelson {michelson; args}) return_ty
let create_contract ?(range = dummy) () ~storage ~code ~delegate ~initial_balance ~initial_storage =
  create ~range (LLTZ.E.Create_contract { storage; code; delegate; initial_balance; initial_storage }) (mk_type ~range (LLTZ.T.Tuple (Row.Node [Row.Leaf (None, mk_type ~range LLTZ.T.Address); Row.Leaf (None,mk_type ~range LLTZ.T.Operation)])))

(* Primitives *)
(* Arity 0 *)
let amount ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Amount, [])) (mk_type ~range LLTZ.T.Mutez)
let balance ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Balance, [])) (mk_type ~range LLTZ.T.Mutez)
let chain_id_prim ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Chain_id, [])) (mk_type ~range LLTZ.T.Chain_id)
let level ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Level, [])) (mk_type ~range LLTZ.T.Nat)
let now ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Now, [])) (mk_type ~range LLTZ.T.Timestamp)
let self ?(range = dummy) str_opt contract_ty = create ~range (LLTZ.E.Prim (LLTZ.P.Self str_opt, [])) contract_ty
let self_address ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Self_address, [])) (mk_type ~range LLTZ.T.Address)
let sender ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Sender, [])) (mk_type ~range LLTZ.T.Address)
let source ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Source, [])) (mk_type ~range LLTZ.T.Address)
let total_voting_power ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Total_voting_power, [])) (mk_type ~range LLTZ.T.Nat)
let empty_bigmap ?(range = dummy) key value = create ~range (LLTZ.E.Prim (LLTZ.P.Empty_bigmap (key, value), [])) (mk_type ~range (LLTZ.T.Big_map (key, value)))
let empty_map ?(range = dummy) key value = create ~range (LLTZ.E.Prim (LLTZ.P.Empty_map (key, value), [])) (mk_type ~range (LLTZ.T.Map (key, value)))
let empty_set ?(range = dummy) ty = create ~range (LLTZ.E.Prim (LLTZ.P.Empty_set ty, [])) (mk_type ~range (LLTZ.T.Set ty))
let nil ?(range = dummy) ty = create ~range (LLTZ.E.Prim (LLTZ.P.Nil ty, [])) (mk_type ~range (LLTZ.T.List ty))
let none ?(range = dummy) ty = create ~range (LLTZ.E.Prim (LLTZ.P.None ty, [])) (mk_type ~range (LLTZ.T.Option ty))
let sapling_empty_state ?(range = dummy) memo = create ~range (LLTZ.E.Prim (LLTZ.P.Sapling_empty_state { memo }, [])) (mk_type ~range (LLTZ.T.Sapling_state { memo }))
let unit_prim ?(range = dummy) () = create ~range (LLTZ.E.Prim (LLTZ.P.Unit, [])) (mk_type ~range LLTZ.T.Unit)

(* Arity 1/2 *)
let car ?(range = dummy) pair = create ~range (LLTZ.E.Prim (LLTZ.P.Car, [pair])) (
  match pair.type_.desc with 
  | LLTZ.T.Tuple (Node (Leaf(_,hd)::_)) -> hd 
  | _ -> raise_s [%message "Expected pair type" (pair.type_ : LLTZ.T.t)])
let cdr ?(range = dummy) pair = create ~range (LLTZ.E.Prim (LLTZ.P.Cdr, [pair])) (
  match pair.type_.desc with 
  | LLTZ.T.Tuple (Node (Leaf(_,_)::Leaf(_,tl)::[])) -> tl 
  | _ -> raise_s [%message "Expected pair type" (pair.type_ : LLTZ.T.t)])
let left ?(range = dummy) (opt1, opt2, ty) value = create ~range (LLTZ.E.Prim (LLTZ.P.Left (opt1, opt2, ty), [value])) (mk_type ~range (LLTZ.T.Or (LLTZ.R.Node [LLTZ.R.Leaf (convert_option opt1, value.type_); LLTZ.R.Leaf (convert_option opt2, ty)])))
let right ?(range = dummy) (opt1, opt2, ty) value = create ~range (LLTZ.E.Prim (LLTZ.P.Right (opt1, opt2, ty), [value])) (mk_type ~range (LLTZ.T.Or (LLTZ.R.Node [LLTZ.R.Leaf (convert_option opt1, ty); LLTZ.R.Leaf (convert_option opt2, value.type_)])))
let some ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Some, [value])) (mk_type ~range (LLTZ.T.Option value.type_))
let abs ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Abs, [value])) (mk_type ~range LLTZ.T.Nat)
let neg ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Neg, [value])) (mk_type ~range 
  (match value.type_.desc with
  | LLTZ.T.Int
  | LLTZ.T.Nat -> LLTZ.T.Int 
  | LLTZ.T.Bls12_381_g1
  | LLTZ.T.Bls12_381_g2
  | LLTZ.T.Bls12_381_fr -> value.type_.desc  (* Return the same type *)
  | _ -> raise_s [%message "Expected int,nat or BLS12-381 field/group element type" (value.type_ : LLTZ.T.t)]
  )
)
let nat_prim ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Nat, [value])) (mk_type ~range LLTZ.T.Nat)
let int_prim ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Int, [value])) (mk_type ~range LLTZ.T.Int)
let bytes_prim ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Bytes, [value])) (mk_type ~range LLTZ.T.Bytes)
let is_nat ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Is_nat, [value])) (mk_type ~range (LLTZ.T.Option (mk_type ~range LLTZ.T.Nat)))

(* comparisons modified to have arity 2 *)
let compare_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Compare, [lhs; rhs])) (mk_type ~range LLTZ.T.Int)
let eq ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Eq, [compare_ ~range lhs rhs])) (mk_type ~range LLTZ.T.Bool)
let neq ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Neq, [compare_ ~range lhs rhs])) (mk_type ~range LLTZ.T.Bool)
let le ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Le, [compare_ ~range lhs rhs])) (mk_type ~range LLTZ.T.Bool)
let lt ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Lt, [compare_ ~range lhs rhs])) (mk_type ~range LLTZ.T.Bool)
let ge ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Ge, [compare_ ~range lhs rhs])) (mk_type ~range LLTZ.T.Bool)
let gt ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Gt, [compare_ ~range lhs rhs])) (mk_type ~range LLTZ.T.Bool)

let not ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Not, [value])) (mk_type ~range 
  (match value.type_.desc with
  | LLTZ.T.Bool -> LLTZ.T.Bool
  | LLTZ.T.Nat -> LLTZ.T.Int
  | LLTZ.T.Int -> LLTZ.T.Int
  | LLTZ.T.Bytes -> LLTZ.T.Bytes
  | _ -> raise_s [%message "Expected bool, nat, int, or bytes type" (value.type_ : LLTZ.T.t)]
  )
)
let size ?(range = dummy) container = create ~range (LLTZ.E.Prim (LLTZ.P.Size, [container])) (mk_type ~range LLTZ.T.Nat)
let address ?(range = dummy) contract = create ~range (LLTZ.E.Prim (LLTZ.P.Address, [contract])) (mk_type ~range LLTZ.T.Address)
let implicit_account ?(range = dummy) key_hash = create ~range (LLTZ.E.Prim (LLTZ.P.Implicit_account, [key_hash])) (mk_type ~range (LLTZ.T.Contract (mk_type ~range LLTZ.T.Unit)))
let contract ?(range = dummy) (opt, ty) address = create ~range (LLTZ.E.Prim (LLTZ.P.Contract (opt, ty), [address])) (mk_type ~range (LLTZ.T.Option (mk_type ~range (LLTZ.T.Contract ty))))
let pack ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Pack, [value])) (mk_type ~range LLTZ.T.Bytes)
let unpack ?(range = dummy) ty value = create ~range (LLTZ.E.Prim (LLTZ.P.Unpack ty, [value])) (mk_type ~range (LLTZ.T.Option ty))
let hash_key ?(range = dummy) key = create ~range (LLTZ.E.Prim (LLTZ.P.Hash_key, [key])) (mk_type ~range LLTZ.T.Key_hash)
let blake2b ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Blake2b, [bytes])) (mk_type ~range LLTZ.T.Bytes)
let sha256 ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Sha256, [bytes])) (mk_type ~range LLTZ.T.Bytes)
let sha512 ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Sha512, [bytes])) (mk_type ~range LLTZ.T.Bytes)
let keccak ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Keccak, [bytes])) (mk_type ~range LLTZ.T.Bytes)
let sha3 ?(range = dummy) bytes = create ~range (LLTZ.E.Prim (LLTZ.P.Sha3, [bytes])) (mk_type ~range LLTZ.T.Bytes)
let set_delegate ?(range = dummy) delegate = create ~range (LLTZ.E.Prim (LLTZ.P.Set_delegate, [delegate])) (mk_type ~range LLTZ.T.Operation)
let read_ticket ?(range = dummy) ticket = create ~range (LLTZ.E.Prim (LLTZ.P.Read_ticket, [ticket]))
    (mk_type ~range 
      (match ticket.type_.desc with
      | LLTZ.T.Ticket cty -> 
          LLTZ.T.Tuple (LLTZ.R.Node [
            LLTZ.R.Leaf (None,mk_type ~range LLTZ.T.Address); (* Address part of the ticket *)
            LLTZ.R.Leaf (None,cty);            (* The content type of the ticket *)
            LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Nat);     (* The amount (Nat) part of the ticket *)
            LLTZ.R.Leaf (None, mk_type ~range (LLTZ.T.Ticket cty)) (* The original ticket type *)
          ])
      | _ -> raise_s [%message "Expected ticket type" (ticket.type_ : LLTZ.T.t)]
      )
    )
let join_tickets ?(range = dummy) ticket1 ticket2 = create ~range (LLTZ.E.Prim (LLTZ.P.Join_tickets, [ticket1; ticket2])) 
  (mk_type ~range 
    (match (ticket1.type_.desc, ticket2.type_.desc) with
    | (LLTZ.T.Ticket cty1, LLTZ.T.Ticket _) -> 
        LLTZ.T.Option (mk_type ~range (LLTZ.T.Ticket cty1))
    | _ -> raise_s [%message "Expected two tickets of the same type" 
                        (ticket1.type_ : LLTZ.T.t) 
                        (ticket2.type_ : LLTZ.T.t)]
    )
  )
let pairing_check ?(range = dummy) pairings = create ~range (LLTZ.E.Prim (LLTZ.P.Pairing_check, [pairings])) (mk_type ~range LLTZ.T.Bool)
let voting_power ?(range = dummy) key_hash = create ~range (LLTZ.E.Prim (LLTZ.P.Voting_power, [key_hash])) (mk_type ~range LLTZ.T.Nat)
let getn ?(range = dummy) n value = create ~range (LLTZ.E.Prim (LLTZ.P.Get_n n, [value])) (assert false) (*TODO*)
let cast ?(range = dummy) ty value = create ~range (LLTZ.E.Prim (LLTZ.P.Cast ty, [value])) ty
(*let rename = assert false*)
let emit ?(range = dummy) (opt, ty) value = create ~range (LLTZ.E.Prim (LLTZ.P.Emit (opt, ty), [value]))  (mk_type ~range LLTZ.T.Operation)
let failwith ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Failwith, [value])) (mk_type ~range LLTZ.T.Unit) (*output type useless*)
let never ?(range = dummy) value = create ~range (LLTZ.E.Prim (LLTZ.P.Never, [value])) (mk_type ~range LLTZ.T.Unit) (*output type useless*)
let pair ?(range = dummy) (opt1, opt2) first second = create ~range (LLTZ.E.Prim (LLTZ.P.Pair (opt1, opt2), [first; second])) (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [LLTZ.R.Leaf (convert_option opt1, first.type_); LLTZ.R.Leaf (convert_option opt2, second.type_)])))
let add ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Add, [lhs; rhs])) 
  (mk_type ~range 
    (match (lhs.type_.desc, rhs.type_.desc) with
    | (LLTZ.T.Nat, LLTZ.T.Nat) -> LLTZ.T.Nat
    | (LLTZ.T.Nat, LLTZ.T.Int) -> LLTZ.T.Int
    | (LLTZ.T.Int, LLTZ.T.Nat) -> LLTZ.T.Int
    | (LLTZ.T.Int, LLTZ.T.Int) -> LLTZ.T.Int
    | (LLTZ.T.Timestamp, LLTZ.T.Int) -> LLTZ.T.Timestamp
    | (LLTZ.T.Int, LLTZ.T.Timestamp) -> LLTZ.T.Timestamp
    | (LLTZ.T.Mutez, LLTZ.T.Mutez) -> LLTZ.T.Mutez
    | (LLTZ.T.Bls12_381_g1, LLTZ.T.Bls12_381_g1) -> LLTZ.T.Bls12_381_g1
    | (LLTZ.T.Bls12_381_g2, LLTZ.T.Bls12_381_g2) -> LLTZ.T.Bls12_381_g2
    | (LLTZ.T.Bls12_381_fr, LLTZ.T.Bls12_381_fr) -> LLTZ.T.Bls12_381_fr
    | _ -> raise_s [%message "Expected matching types for ADD operation" 
                        (lhs.type_ : LLTZ.T.t) 
                        (rhs.type_ : LLTZ.T.t)]
    )
  )
let mul ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Mul, [lhs; rhs]))
  (mk_type ~range 
    (match (lhs.type_.desc, rhs.type_.desc) with
    | (LLTZ.T.Nat, LLTZ.T.Nat) -> LLTZ.T.Nat
    | (LLTZ.T.Nat, LLTZ.T.Int) -> LLTZ.T.Int
    | (LLTZ.T.Int, LLTZ.T.Nat) -> LLTZ.T.Int
    | (LLTZ.T.Int, LLTZ.T.Int) -> LLTZ.T.Int
    | (LLTZ.T.Mutez, LLTZ.T.Nat) -> LLTZ.T.Mutez
    | (LLTZ.T.Nat, LLTZ.T.Mutez) -> LLTZ.T.Mutez
    | (LLTZ.T.Bls12_381_g1, LLTZ.T.Bls12_381_fr) -> LLTZ.T.Bls12_381_g1
    | (LLTZ.T.Bls12_381_g2, LLTZ.T.Bls12_381_fr) -> LLTZ.T.Bls12_381_g2
    | (LLTZ.T.Bls12_381_fr, LLTZ.T.Bls12_381_fr) -> LLTZ.T.Bls12_381_fr
    | (LLTZ.T.Nat, LLTZ.T.Bls12_381_fr) -> LLTZ.T.Bls12_381_fr
    | (LLTZ.T.Int, LLTZ.T.Bls12_381_fr) -> LLTZ.T.Bls12_381_fr
    | (LLTZ.T.Bls12_381_fr, LLTZ.T.Nat) -> LLTZ.T.Bls12_381_fr
    | (LLTZ.T.Bls12_381_fr, LLTZ.T.Int) -> LLTZ.T.Bls12_381_fr
    | _ -> raise_s [%message "Expected matching types for MUL operation" 
                        (lhs.type_ : LLTZ.T.t) 
                        (rhs.type_ : LLTZ.T.t)]
    )
  )

let sub ?(range = dummy) (lhs:Expr.t) (rhs:Expr.t) = 
  match (lhs.type_.desc, rhs.type_.desc) with
  | (LLTZ.T.Mutez, LLTZ.T.Mutez) -> (
      (*if_none*) (create ~range (LLTZ.E.Prim (LLTZ.P.Sub_mutez, [lhs; rhs])) (mk_type ~range (LLTZ.T.Option (mk_type ~range LLTZ.T.Mutez))))
       (* ~some:(
        let var_name = Name.create () in  
        {lam_var=(Var var_name, mutez_ty ~range ()); body = variable ~range (Var var_name) (mutez_ty ~range ())})
        ~none:(failwith ~range (string ~range "SUB_MUTEZ underflow"))*)
  )
  | _ -> 
      create ~range (LLTZ.E.Prim (LLTZ.P.Sub, [lhs; rhs]))
        (mk_type ~range 
          (match (lhs.type_.desc, rhs.type_.desc) with
          | (LLTZ.T.Nat, LLTZ.T.Nat) -> LLTZ.T.Int
          | (LLTZ.T.Nat, LLTZ.T.Int) -> LLTZ.T.Int
          | (LLTZ.T.Int, LLTZ.T.Nat) -> LLTZ.T.Int
          | (LLTZ.T.Int, LLTZ.T.Int) -> LLTZ.T.Int
          | (LLTZ.T.Timestamp, LLTZ.T.Int) -> LLTZ.T.Timestamp
          | (LLTZ.T.Timestamp, LLTZ.T.Timestamp) -> LLTZ.T.Int
          | _ -> raise_s [%message "Expected matching types for SUB operation" 
                          (lhs.type_ : LLTZ.T.t) 
                          (rhs.type_ : LLTZ.T.t)]
          )
        )

let sub_mutez ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Sub_mutez, [lhs; rhs])) (mk_type ~range (LLTZ.T.Option (mk_type ~range LLTZ.T.Mutez)))
let lsr_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Lsr, [lhs; rhs])) lhs.type_
let lsl_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Lsl, [lhs; rhs])) lhs.type_
let xor ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Xor, [lhs; rhs])) lhs.type_
let ediv ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Ediv, [lhs; rhs])) 
    (mk_type ~range 
      (match (lhs.type_.desc, rhs.type_.desc) with
      | (LLTZ.T.Nat, LLTZ.T.Nat) -> LLTZ.T.Option (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [LLTZ.R.Leaf (None,mk_type ~range LLTZ.T.Nat);LLTZ.R.Leaf (None,mk_type ~range LLTZ.T.Nat)])))
      | (LLTZ.T.Nat, LLTZ.T.Int) 
      | (LLTZ.T.Int, LLTZ.T.Nat) 
      | (LLTZ.T.Int, LLTZ.T.Int) -> LLTZ.T.Option (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [LLTZ.R.Leaf (None,mk_type ~range LLTZ.T.Int);LLTZ.R.Leaf (None,mk_type ~range LLTZ.T.Nat)])))
      | (LLTZ.T.Mutez, LLTZ.T.Nat) -> LLTZ.T.Option (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Mutez);LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Mutez)])))
      | (LLTZ.T.Mutez, LLTZ.T.Mutez) -> LLTZ.T.Option (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Nat);LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Mutez)])))
      | _ -> raise_s [%message "Expected matching types for EDIV operation" 
                          (lhs.type_ : LLTZ.T.t) 
                          (rhs.type_ : LLTZ.T.t)]
      )
    )

let div_ ?(range = dummy) (lhs:Expr.t) (rhs:Expr.t) =
  (if_none ~range (ediv ~range lhs rhs) ~some:(
  let var_ty = tuple_ty ~range (Row.Node [Row.Leaf (None, lhs.type_); Row.Leaf(None, rhs.type_)]) in
  let var_name = Name.create () in
  {lam_var=(Var var_name, var_ty ); 
  body = car (variable ~range (Var var_name) (var_ty ))})
  ~none:(failwith ~range (string ~range "DIV by 0")))

let mod_ ?(range = dummy) (lhs:Expr.t) (rhs:Expr.t) =
    (if_none ~range (ediv ~range lhs rhs) ~some:(
    let var_ty = tuple_ty ~range (Row.Node [Row.Leaf (None, lhs.type_); Row.Leaf(None, rhs.type_)]) in
    let var_name = Name.create () in
    {lam_var=(Var var_name, var_ty ); 
    body = cdr (variable ~range (Var var_name) (var_ty ))})
    ~none:(failwith ~range (string ~range "MOD by 0")))

let and_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.And, [lhs; rhs])) rhs.type_
let or_ ?(range = dummy) lhs rhs = create ~range (LLTZ.E.Prim (LLTZ.P.Or, [lhs; rhs])) lhs.type_
let cons ?(range = dummy) head tail = create ~range (LLTZ.E.Prim (LLTZ.P.Cons, [head; tail])) tail.type_
let concat1 ?(range = dummy) val1 str2 = create ~range (LLTZ.E.Prim (LLTZ.P.Concat1, [val1; str2])) (mk_type ~range LLTZ.T.String)
let concat2 ?(range = dummy) val1 bytes2 = create ~range (LLTZ.E.Prim (LLTZ.P.Concat2, [val1; bytes2])) (mk_type ~range LLTZ.T.Bytes)
let get ?(range = dummy) key collection = create ~range (LLTZ.E.Prim (LLTZ.P.Get, [key; collection]))
  (mk_type ~range 
    (match collection.type_.desc with
    | LLTZ.T.Map (_, vty) 
    | LLTZ.T.Big_map (_, vty) -> LLTZ.T.Option vty
    | _ -> raise_s [%message "Expected a map or big_map with matching key type" 
                        (key.type_ : LLTZ.T.t) 
                        (collection.type_ : LLTZ.T.t)]
    )
  )
let mem ?(range = dummy) key collection = create ~range (LLTZ.E.Prim (LLTZ.P.Mem, [key; collection])) (mk_type ~range LLTZ.T.Bool)
let exec ?(range = dummy) value lambda = create ~range (LLTZ.E.Prim (LLTZ.P.Exec, [value; lambda])) (
  match lambda.type_.desc with 
  |LLTZ.T.Function (_, ret_ty) -> ret_ty 
  | _ -> raise_s [%message "Expected function type" (lambda.type_ : LLTZ.T.t)])
let apply ?(range = dummy) value lambda= create ~range (LLTZ.E.Prim (LLTZ.P.Apply, [value; lambda]))
  (match lambda.type_.desc with 
  | LLTZ.T.Function ({ desc = LLTZ.T.Tuple (
      LLTZ.R.Node [
        LLTZ.R.Leaf (None, _);
        LLTZ.R.Leaf (None, ty2)
      ]
  ); range = _ }, ty3) -> (mk_type ~range (LLTZ.T.Function (ty2,ty3)))
  | _ -> raise_s [%message "Expected function type" (lambda.type_ : LLTZ.T.t)])
let sapling_verify_update ?(range = dummy) transaction state = create ~range (LLTZ.E.Prim (LLTZ.P.Sapling_verify_update, [transaction; state]))
  (match (transaction.type_.desc, state.type_.desc) with
  | (LLTZ.T.Sapling_transaction _, LLTZ.T.Sapling_state ms2) ->
      mk_type ~range (LLTZ.T.Option (
        mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [
          LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Bytes);
          LLTZ.R.Leaf (None, mk_type ~range LLTZ.T.Int);
          LLTZ.R.Leaf (None, mk_type ~range (LLTZ.T.Sapling_state ms2))
        ]
      ))
    )
  )
  | _ -> raise_s [%message "Expected matching sapling_transaction and sapling_state types" 
                      (transaction.type_ : LLTZ.T.t) 
                      (state.type_ : LLTZ.T.t)]
)
let ticket ?(range = dummy) content amount = create ~range (LLTZ.E.Prim (LLTZ.P.Ticket, [content; amount])) (mk_type ~range (LLTZ.T.Option (mk_type ~range (LLTZ.T.Ticket content.type_))))
let ticket_deprecated ?(range = dummy) content amount = create ~range (LLTZ.E.Prim (LLTZ.P.Ticket_deprecated, [content; amount])) (mk_type ~range (LLTZ.T.Option (mk_type ~range (LLTZ.T.Ticket content.type_))))
let split_ticket ?(range = dummy) ticket amounts = create ~range (LLTZ.E.Prim (LLTZ.P.Split_ticket, [ticket; amounts])) 
  (mk_type ~range 
        (LLTZ.T.Option (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [
          LLTZ.R.Leaf (None, mk_type ~range (LLTZ.T.Ticket ticket.type_));
          LLTZ.R.Leaf (None, mk_type ~range (LLTZ.T.Ticket ticket.type_))
        ])))))
let updaten ?(range = dummy) n value pair = create ~range (LLTZ.E.Prim (LLTZ.P.Update_n n, [value; pair])) (assert false) (*TODO*)
let view ?(range = dummy) name ~return_type ~d ~address = create ~range (LLTZ.E.Prim (LLTZ.P.View (name, return_type), [d; address])) (mk_type ~range (LLTZ.T.Option return_type))

(* Arity 3 *)
let slice ?(range = dummy) offset ~length ~seq = create ~range (LLTZ.E.Prim (LLTZ.P.Slice, [offset; length; seq])) (mk_type ~range (LLTZ.T.Option seq.type_))
let update ?(range = dummy) key value ~of_ = create ~range (LLTZ.E.Prim (LLTZ.P.Update, [key; value; of_])) of_.type_
let get_and_update ?(range = dummy) key value ~of_ = create ~range (LLTZ.E.Prim (LLTZ.P.Get_and_update, [key; value; of_])) (mk_type ~range (LLTZ.T.Tuple (LLTZ.R.Node [LLTZ.R.Leaf (None, value.type_); LLTZ.R.Leaf (None, of_.type_)])))
let transfer_tokens ?(range = dummy) param ~amount ~contract = create ~range (LLTZ.E.Prim (LLTZ.P.Transfer_tokens, [param; amount; contract])) (mk_type ~range LLTZ.T.Operation)
let check_signature ?(range = dummy) key ~signature ~message = create ~range (LLTZ.E.Prim (LLTZ.P.Check_signature, [key; signature; message])) (mk_type ~range LLTZ.T.Bool)
let open_chest ?(range = dummy) chest_key ~chest ~time = create ~range (LLTZ.E.Prim (LLTZ.P.Open_chest, [chest_key; chest; time])) (mk_type ~range (LLTZ.T.Option (mk_type ~range LLTZ.T.Bytes)))

let convert_list (exprs: LLTZ.E.t list) : LLTZ.E.t Row.t =
  let converted_row_leaves = List.map ~f:(fun expr -> Row.Leaf (None, expr)) exprs in
  Row.Node converted_row_leaves

let gen_name () = Name.create ()
let annon_function var_name var_ty ~body : LLTZ.E.lambda = { lam_var = (Var (var_name), var_ty); body }