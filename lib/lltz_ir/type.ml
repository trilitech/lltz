open Core
open Grace

type t =
  { desc : desc
  ; range : Range.t
  }

and annot = string
and row = (annot * t) Rose_tree.t

and desc =
  (* sum and product types *)
  | Tuple of row
  | Or of row
  (* parameterized types *)
  | Option of t
  | List of t
  | Set of t
  | Contract of t
  | Ticket of t
  | Function of t * t
  | Map of t * t
  | Big_map of t * t
  (* primitive types *)
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
[@@deriving sexp, equal, compare]
