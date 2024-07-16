open Lltz_ir
open Michel
open Michelson_base

module M = Michel.Expr
module L = Lltz_ir.Expr


module LP = Lltz_ir.Primitive
module LT = Lltz_ir.Type
module LR = Lltz_ir.Row
module MBT = Michelson_base.Type

open Utils
open Control
open Type
open Format
open Grace

let dummy : Range.t = { start = 0; stop=0; source = "";}

(* Type conversion*)
(* Conversion function from M.ty to L.Base.t *)
let rec convert_ty (m_ty : Michel.Type.ty) : LT.t =
  match m_ty with
  | Michel.Type.T0 t0 -> convert_t0 t0
  | Michel.Type.T1 (t1, ty) -> convert_t1 t1 (convert_ty ty)
  | Michel.Type.T2 (t2, ty1, ty2) -> convert_t2 t2 (convert_ty ty1) (convert_ty ty2)
  (*| Michel.Type.Record r -> LT.Tuple (convert_row r)
  | Michel.Type.Variant r -> LT.Or (convert_row r)
  | Michel.Type.Vector tys -> LT.List (LT.Tuple (convert_vector tys))*)
  | Michel.Type.Missing s -> failwith ("Cannot convert missing type: " ^ s)

(* Conversion function for M.T0 to L.Base.t *)
and convert_t0 (t0 : MBT.type0) : LT.t =
  let mk_type ty : LT.t = { desc = ty; range = dummy } in
  mk_type @@
  match t0 with
  | Unit -> LT.Unit
  | Bool -> LT.Bool
  | Nat -> LT.Nat
  | Int -> LT.Int
  | Mutez -> LT.Mutez
  | String -> LT.String
  | Bytes -> LT.Bytes
  | Chain_id -> LT.Chain_id
  | Timestamp -> LT.Timestamp
  | Address -> LT.Address
  | Key -> LT.Keys
  | Key_hash -> LT.Key_hash
  | Signature -> LT.Signature
  | Operation -> LT.Operation
  | Sapling_state { memo } -> LT.Sapling_state { memo }
  | Sapling_transaction { memo } -> LT.Sapling_transaction { memo }
  | Never -> LT.Never
  | Bls12_381_g1 -> LT.Bls12_381_g1
  | Bls12_381_g2 -> LT.Bls12_381_g2
  | Bls12_381_fr -> LT.Bls12_381_fr
  | _ -> failwith "Unsupported T0 type"

(* Conversion function for M.T1 to L.Base.t *)
and convert_t1 (t1 : MBT.type1) (ty2 : LT.t)  =
  let mk_type ty : LT.t = { desc = ty; range = dummy } in
  mk_type @@
  match t1 with
  | List -> LT.List (ty2)
  | Set -> LT.Set ty2
  | Contract -> LT.Contract ty2
  | Ticket -> LT.Ticket ty2
  | Option -> LT.Option ty2

(* Conversion function for M.T2 to L.Base.t *)
and convert_t2 (t2 : MBT.type2) (ty1 : LT.t) (ty2 : LT.t) : LT.t =
  let mk_type ty : LT.t = { desc = ty; range = dummy } in
  mk_type @@
  match t2 with
  | Map -> LT.Map (ty1, ty2)
  | Big_map -> LT.Big_map (ty1, ty2)
  | Lambda -> LT.Function (ty1, ty2)
  | _ -> failwith "Unsupported T2 type"

(* Conversion function for M.row to L.row *)
(*and convert_row (row : (string option * M.ty) Binary_tree.t) : LT.row =
  let convert_leaf (lbl, ty) = (lbl, convert_ty ty) in
  Rose_tree.map convert_leaf row

(* Conversion for vector of types *)
and convert_vector (tys : M.ty list) : LT.row =
  let annotated_list = List.map (fun ty -> (None, convert_ty ty)) tys in
  Rose_tree.Leaf (annotated_list)*)



(* Helper function to map Michelle literal to LLTZ-IR constant *)
let translate_literal = function
  | M.Unit -> L.Unit
  | M.Bool b -> L.Bool b
  | M.Nat n -> L.Nat (Z.of_int (Bigint.int_of_big_int n))
  | M.Int i -> L.Int (Z.of_int (Bigint.int_of_big_int i))
  | M.Mutez m -> L.Mutez (Z.of_int (Bigint.int_of_big_int m))
  | M.String s -> L.String s
  | M.Key k -> L.Key k
  | M.Key_hash kh -> L.Key_hash kh
  | M.Bytes b -> L.Bytes b
  | M.Chain_id cid -> L.Chain_id cid
  | M.Address addr -> L.Address addr
  | M.Timestamp ts -> L.Timestamp ts
  | M.Bls12_381_g1 g1 -> L.Bls12_381_g1 g1
  | M.Bls12_381_g2 g2 -> L.Bls12_381_g2 g2
  | M.Bls12_381_fr fr -> L.Bls12_381_fr fr
  | M.Signature s -> L.Signature s

(* Helper function to map Michelle primitive to LLTZ-IR primitive *)
let translate_prim0 (op : Michel.Type.ty M.prim0) : LP.t =
  match op with
  | M.Amount -> LP.Amount
  | M.Balance -> LP.Balance
  | M.Chain_id -> LP.Chain_id
  | M.Level -> LP.Level
  | M.Now -> LP.Now
  | M.Self None -> LP.Self None
  | M.Self (Some x) -> LP.Self (Some x)
  | M.Self_address -> LP.Self_address
  | M.Sender -> LP.Sender
  | M.Source -> LP.Source
  | M.Total_voting_power -> LP.Total_voting_power
  | M.Empty_bigmap (t1, t2) -> LP.Empty_bigmap (convert_ty t1, convert_ty t2)
  | M.Empty_map (t1, t2) -> LP.Empty_map (convert_ty t1, convert_ty t2)
  | M.Empty_set t -> LP.Empty_set (convert_ty t)
  | M.Nil t -> LP.Nil (convert_ty t)
  | M.None_ t -> LP.None (convert_ty t)
  | M.Sapling_empty_state {memo:int} -> LP.Sapling_empty_state {memo:int}
  | M.Unit_ -> LP.Unit

let translate_prim1 (op : Michel.Type.ty M.prim1) : LP.t =
  match op with
  | M.Car -> LP.Car
  | M.Cdr -> LP.Cdr
  | M.Left (annot1, annot2, t) -> LP.Left (annot1, annot2, convert_ty t)
  | M.Right (annot1, annot2, t) -> LP.Right (annot1, annot2, convert_ty t)
  | M.Some_ -> LP.Some
  | M.Eq -> LP.Eq
  | M.Abs -> LP.Abs
  | M.Neg -> LP.Neg
  | M.Nat -> LP.Nat
  | M.Int -> LP.Int
  | M.Bytes -> LP.Bytes
  | M.IsNat -> LP.Is_nat
  | M.Neq -> LP.Neq
  | M.Le -> LP.Le
  | M.Lt -> LP.Lt
  | M.Ge -> LP.Ge
  | M.Gt -> LP.Gt
  | M.Not -> LP.Not
  | M.Concat1 -> LP.Concat1
  | M.Size -> LP.Size
  | M.Address -> LP.Address
  | M.Implicit_account -> LP.Implicit_account
  | M.Contract (annot, t) -> LP.Contract (annot, convert_ty t)
  | M.Pack -> LP.Pack
  | M.Unpack t -> LP.Unpack (convert_ty t)
  | M.Hash_key -> LP.Hash_key
  | M.Blake2b -> LP.Blake2b
  | M.Sha256 -> LP.Sha256
  | M.Sha512 -> LP.Sha512
  | M.Keccak -> LP.Keccak
  | M.Sha3 -> LP.Sha3
  | M.Set_delegate -> LP.Set_delegate
  | M.Read_ticket -> LP.Read_ticket
  | M.Join_tickets -> LP.Join_tickets
  | M.Pairing_check -> LP.Pairing_check
  | M.Voting_power -> LP.Voting_power
  | M.Getn n -> LP.Getn n
  | M.Cast t -> LP.Cast (convert_ty t)
  | M.Rename annot -> LP.Rename annot
  | M.Emit (annot, t_opt) -> LP.Emit (annot, Option.map convert_ty t_opt)

let translate_prim1_fail = function
  | M.Failwith -> LP.Failwith
  | M.Never -> LP.Never

let translate_prim2 (op : Michel.Type.ty M.prim2) : LP.t =
  match op with
  | M.Pair (annot1, annot2) -> LP.Pair (annot1, annot2)
  | M.Add -> LP.Add
  | M.Mul -> LP.Mul
  | M.Sub -> LP.Sub
  | M.Sub_mutez -> LP.Sub_mutez
  | M.Lsr -> LP.Lsr
  | M.Lsl -> LP.Lsl
  | M.Xor -> LP.Xor
  | M.Ediv -> LP.Ediv
  | M.And -> LP.And
  | M.Or -> LP.Or
  | M.Cons -> LP.Cons
  | M.Compare -> LP.Compare
  | M.Concat2 -> LP.Concat2
  | M.Get -> LP.Get
  | M.Mem -> LP.Mem
  | M.Exec -> LP.Exec
  | M.Apply -> LP.Apply
  | M.Sapling_verify_update -> LP.Sapling_verify_update
  | M.Ticket -> LP.Ticket
  | M.Ticket_deprecated -> LP.Ticket_deprecated
  | M.Split_ticket -> LP.Split_ticket
  | M.Updaten n -> LP.Updaten n
  | M.View (name, t) -> LP.View (name, convert_ty t)

let translate_prim3 (op : M.prim3) : LP.t =
  match op with
  | M.Slice -> LP.Slice
  | M.Update -> LP.Update
  | M.Get_and_update -> LP.Get_and_update
  | M.Transfer_tokens -> LP.Transfer_tokens
  | M.Check_signature -> LP.Check_signature
  | M.Open_chest -> LP.Open_chest

(* translate Michelle type to LLTZ type *)
(*let rec translate_type = function
  | M.Ty_unit -> L.Type.unit
  | M.Ty_bool -> L.Type.bool
  | M.Ty_nat -> L.Type.nat
  | M.Ty_int -> L.Type.int
  | M.Ty_mutez -> L.Type.mutez
  | M.Ty_string -> L.Type.string
  | M.Ty_bytes -> L.Type.bytes
  | M.Ty_chain_id -> L.Type.chain_id
  | M.Ty_key -> L.Type.key
  | M.Ty_key_hash -> L.Type.key_hash
  | M.Ty_address -> L.Type.address
  | M.Ty_timestamp -> L.Type.timestamp
  | M.Ty_bls12_381_g1 -> L.Type.bls12_381_g1
  | M.Ty_bls12_381_g2 -> L.Type.bls12_381_g2
  | M.Ty_bls12_381_fr -> L.Type.bls12_381_fr
  | M.Ty_signature -> L.Type.signature
  | M.Ty_option t -> L.Type.option (translate_type t)
  | M.Ty_list t -> L.Type.list (translate_type t)
  | M.Ty_set t -> L.Type.set (translate_type t)
  | M.Ty_map (kt, vt) -> L.Type.map (translate_type kt) (translate_type vt)
  | M.Ty_big_map (kt, vt) -> L.Type.big_map (translate_type kt) (translate_type vt)
  | M.Ty_pair (t1, t2) -> L.Type.pair (translate_type t1) (translate_type t2)
  | M.Ty_or (t1, t2) -> L.Type.or_ (translate_type t1) (translate_type t2)
  | M.Ty_lambda (param, ret) -> L.Type.lambda (translate_type param) (translate_type ret)
  | M.Ty_contract t -> L.Type.contract (translate_type t)*)

(* Main function to translate Michelle expression to LLTZ-IR expression *) (*'a M.expr_f -> 'b list L.t*)
let rec translate_expr (expr : M.expr) : L.t =
  let mk_expr expr : L.t = { desc = expr; range = dummy } in 
  mk_expr @@
  match expr.expr with
  | M.Var v -> L.Var (L.Variable v)
  | M.Let_in (p, e1, e2) ->
    let v = match p with 
    | M.P_var (Some x) -> x  
      | _ -> failwith "Unsupported pattern" 
    in
    (* Convert `v` to `Base.var` and then construct the record for `L.Let_in` *)
    L.Let_in { 
      let_var = L.Variable v; 
      rhs = translate_expr e1; 
      in_ = translate_expr e2 
    }
  | M.Lambda (p, t1, _, e) ->
      let v = match p with Some x -> x | None -> failwith "unsupported pattern" in
      L.Lambda {lam_var = L.Variable v ; return = (convert_ty t1); body = translate_expr e}
  | M.Lambda_rec (p1, p2, t1, _, e) ->
      let f = match p1 with Some x -> x | None -> failwith "unsupported pattern" in
      let x = match p2 with Some x -> x | None -> failwith "unsupported pattern" in
      L.Lambda_rec {lam_var = L.Variable f; mu_var = L.Variable x; return = convert_ty t1; body = translate_expr e}
  | M.Lit l -> L.Const (translate_literal l)
  | M.Prim0 op -> L.Prim ((translate_prim0 op), [])
  | M.Prim1 (op, e) -> L.Prim ((translate_prim1 op), [translate_expr e])
  | M.Prim1_fail (op, e) -> L.Prim (translate_prim1_fail op, [translate_expr e]) (* same as Prim1 *)
  | M.Prim2 (op, e1, e2) -> L.Prim (translate_prim2 op, [translate_expr e1; translate_expr e2])
  | M.Prim3 (op, e1, e2, e3) -> L.Prim (translate_prim3 op, [translate_expr e1; translate_expr e2; translate_expr e3])
  | M.Proj_field (fld, e) -> L.Proj (translate_expr e, Here []) (*TODO*)
  | M.Stack_op (op, xs) -> translate_stack_op op xs
  | M.Record r -> L.Tuple (translate_record r)
  | M.Variant (lbl, rc, e) -> translate_variant lbl rc (translate_expr e)

  | M.List (_t, xs) -> L.Tuple (LR. (Node (List.map (fun expr -> Leaf (None, translate_expr expr)) xs)))
  | M.Set (_t, xs) -> 
      let elements = List.map (fun expr -> LR.Leaf (None, translate_expr expr)) xs in
      L.Tuple(LR.(Node elements))
  | M.Map (_kt, _vt, xs) -> 
      let elements = List.map (fun (k, v) -> 
        LR.Node [Leaf (None, translate_expr k); Leaf (None, translate_expr v)]
      ) xs in
      L.Tuple LR. (Node elements)
  | M.Match_record (rp, e1, e2) -> translate_match_record rp (translate_expr e1) (translate_expr e2)
  | M.Match_variant (e, clauses) -> translate_match_variant (translate_expr e) clauses
  | M.Vector es -> L.Tuple (LR.Node (List.map (fun expr -> LR.Leaf (None, translate_expr expr)) es))
  | M.Nth (n, e) -> L.Proj (translate_expr e, LR.Path.Here [n])
  | M.Unpair (n, e) -> L.Prim (LP.Getn n, [translate_expr e])

  | M.If (e1, e2, e3) -> L.If_bool {condition = translate_expr e1; if_true = translate_expr e2; if_false = translate_expr e3}
  | M.If_none (e1, x, e2, e3) -> 
    let v = match x with 
    | (Some x) -> x  
      | _ -> failwith "Unsupported value" 
    in
    L.If_none { subject = translate_expr e1; if_none = translate_expr e2; if_some = (L.Variable v, translate_expr e3) }
  | M.If_left (e, xl, e1, xr, e2) -> 
    let vl = match xl with
    | (Some x) -> x
    | _ -> failwith "Unsupported value" 
    in let vr = match xr with
    | (Some x) -> x
    | _ -> failwith "Unsupported value"
    in
    L.If_left {subject = translate_expr e; if_left = (L.Variable vl, translate_expr e1); if_right = (L.Variable vr, translate_expr e2)}
  | M.If_cons (e, x, xs, e1, e2) -> 
    let v = match x with
    | (Some x) -> x
    | _ -> failwith "Unsupported value"
    in
    let vs = match xs with
    | (Some x) -> x
    | _ -> failwith "Unsupported value"
    in
    L.If_cons {subject = translate_expr e; if_empty = translate_expr e1; if_nonempty = (L.Variable v, L.Variable vs, translate_expr e2)}

  | M.Loop (init, _xs, step) -> 
    let translated_init = List.map translate_expr init in
    L.While_left {invariant = {desc = L.Prim (LP.Not, translated_init); range = dummy}; body = translate_expr step}
  
  | M.Map_over (init, xs, step) -> 
    let translated_init = List.map translate_expr init in
    let vars = List.map (fun x -> match x with Some str -> L.Variable str | None -> L.Variable "none" ) xs in
    L.Map {collection = {desc = L.Prim (LP.Not, translated_init); range = dummy}; (*Invariant needs to be changed*)
      map = (vars, translate_expr step)}

  | M.Iter_over (init, xs, step) ->
    let translated_init = List.map translate_expr init in
    let vars = List.map (fun x -> match x with Some str -> L.Variable str | None -> L.Variable "none" ) xs in
    L.For_each {collection = {desc = L.Prim (LP.Not, translated_init); range = dummy}; 
      indices= vars; body = (translate_expr step)}

  | M.Create_contract (c, _e1, _e2, _e3) -> 
    let parameter = convert_ty (fst c.M.tparameter) in
    let storage = convert_ty c.M.tstorage in
    let code = translate_expr c.M.body in
    L.Create_contract {
      parameter = parameter;
      storage = storage;
      code = code;
    }
  | M.Record_of_tree (lbls, e) -> translate_record_of_tree lbls (translate_expr e)
  | M.Comment (_, e) -> (translate_expr e).desc

and compile_annotated_type_expression (annot_type : I.type_expression I.annotated) =
  let annot, type_ = annot_type in
  compile_annot annot, compile_type_expression type_

and compile_row annot_types =
  let open LR in
  let labelled_types = List.map annot_types ~f:compile_annotated_type_expression in
  Node (List.map labelled_types ~f:(fun (label, type_) -> Leaf (label, type_)))

(* Additional helper functions *)
and translate_stack_op op xs = match op, xs with
(* Placeholders, the LLTZ-IR does not have stack operations*)
  | M.Swap , hd::_ -> L.Prim (LP.Getn 1, [translate_expr hd])
  | M.Dup n, hd::_ -> L.Prim (LP.Getn n, [translate_expr hd])
  | M.Dig n, hd::_ -> L.Prim (LP.Getn n, [translate_expr hd])
  | M.Dug n, hd::_ -> L.Prim (LP.Getn n, [translate_expr hd])
  | M.Drop n, hd::_ -> L.Prim (LP.Getn n, [translate_expr hd])
  | _, [] -> failwith "Empty list"

and translate_record r =
  Binary_tree.map (fun (lbl, e) -> lbl, translate_expr e) r

and translate_variant lbl rc e =
  match lbl with
  | Some l -> L.Inj ([l], e)
  | None -> failwith "anonymous variant not supported"

and translate_match_record rp e1 e2 =
  L.Let_tuple_in (Binary_tree.fold (fun acc lbl -> lbl :: acc) [] rp, e1, e2)

and translate_match_variant e clauses =
  let rec translate_clauses = function
    | Binary_tree.Leaf { M.cons; var; rhs } -> [cons, translate_expr rhs]
    | Binary_tree.Node (l, r) -> translate_clauses l @ translate_clauses r
  in
  L.Match (e, translate_clauses clauses)

and translate_record_of_tree lbls e =
  let lbls = Binary_tree.fold (fun acc lbl -> lbl :: acc) [] lbls in
  L.Tuple (lbls @ [translate_expr e])

(* Top-level function to translate a Michelle program *)
let translate_program (program : M.expr) : L.t =
  translate_expr program
