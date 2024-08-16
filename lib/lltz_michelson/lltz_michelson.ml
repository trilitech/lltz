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

let rec convert_type (ty: LLTZ.T.t) : Michelson.Ast.t =
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
  | Sapling_state {memo} -> Michelson.T.sampling_state (Michelson.Ast.int (memo))
  | Sapling_transaction {memo} -> Michelson.T.sapling_transaction (Michelson.Ast.int memo)
  | Never -> Michelson.T.never
  | Bls12_381_g1 -> Michelson.T.bls12_381_g1
  | Bls12_381_g2 -> Michelson.T.bls12_381_g2
  | Bls12_381_fr -> Michelson.T.bls12_381_fr
  | Chest_key -> Michelson.T.chest_key
  | Chest -> Michelson.T.chest

and convert_constant (const: LLTZ.E.constant) : Michelson.Ast.t =
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

  let get_const_type (const: LLTZ.E.constant) : Michelson.Ast.t =
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

let convert_primitive (prim: LLTZ.P.t) : Michelson.Ast.t =
  let open Michelson.Ast.Instruction in
    match prim with
    | Amount -> amount
    | Balance -> balance
    | Chain_id -> chain_id
    | Level -> level
    | Now -> now
    | Self opt -> failwith
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
    | Left (opt1, opt2, ty) -> left (convert_type ty)
    | Right (opt1, opt2, ty) -> right (convert_type ty)
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
    | Contract (opt, ty) -> contract (convert_type ty)
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
    | Rename opt -> failwith 
    | Emit (opt, ty_opt) -> failwith
    | Failwith -> failwith
    | Never -> never
    | Pair (opt1, opt2) -> pair
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
    | View (name, ty) -> failwith
    | Slice -> slice
    | Update -> update
    | Get_and_update -> get_and_update
    | Transfer_tokens -> transfer_tokens
    | Check_signature -> check_signature
    | Open_chest -> open_chest
