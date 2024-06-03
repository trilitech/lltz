open Common

type 'ty row = (annot, 'ty) Rose_tree.t
type row_path = annot Rose_tree.path

type 'ty t =
  (* sum and product types *)
  | Tuple of 'ty row
  | Or of 'ty row
  (* parameterized types *)
  | Option of 'ty
  | List of 'ty
  | Set of 'ty
  | Contract of 'ty
  | Ticket of 'ty
  | Function of 'ty * 'ty
  | Map of 'ty * 'ty
  | Big_map of 'ty * 'ty
  (* primitive types  *)
  | Unit
  | Bool
  | Nat
  | Int
  | Mutez
  | String
  | Bytes
  | Chain_id
  | Timestamp
  | Address
  | Key
  | Key_hash
  | Signature
  | Operation
  | Sapling_state of { memo : int }
  | Sapling_transaction of { memo : int }
  | Never
  | Bls12_381_g1
  | Bls12_381_g2
  | Bls12_381_fr
  | Chest_key
  | Chest

let map (_ : 'a -> 'b) (_ : 'a t) : 'b t = failwith "TODO"
