(* 
  lltz_michelson.ml 
  Compiles types, constants, primitives and expressions from LLTZ-IR to Michelson Ast.
*) 

module LLTZ = struct
  module E = Lltz_ir.Expr
  module T = Lltz_ir.Type
  module R = Lltz_ir.Row
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
