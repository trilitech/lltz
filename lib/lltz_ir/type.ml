open Core
open Common

type 'ty row = (label, 'ty) Rose_tree.t [@@deriving sexp, equal, compare]
type row_path = label Rose_tree.Path.t [@@deriving sexp, equal, compare]

module T = struct
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

  let map t ~f =
    match t with
    | Tuple row -> Tuple (Rose_tree.map row ~f)
    | Or row -> Or (Rose_tree.map row ~f)
    | Option ty -> Option (f ty)
    | List ty -> List (f ty)
    | Set ty -> Set (f ty)
    | Contract ty -> Contract (f ty)
    | Ticket ty -> Ticket (f ty)
    | Function (ty1, ty2) -> Function (f ty1, f ty2)
    | Map (ty1, ty2) -> Map (f ty1, f ty2)
    | Big_map (ty1, ty2) -> Big_map (f ty1, f ty2)
    | Unit -> Unit
    | Bool -> Bool
    | Nat -> Nat
    | Int -> Int
    | Mutez -> Mutez
    | String -> String
    | Bytes -> Bytes
    | Chain_id -> Chain_id
    | Timestamp -> Timestamp
    | Address -> Address
    | Key -> Key
    | Key_hash -> Key_hash
    | Signature -> Signature
    | Operation -> Operation
    | Sapling_state { memo } -> Sapling_state { memo }
    | Sapling_transaction { memo } -> Sapling_transaction { memo }
    | Never -> Never
    | Bls12_381_g1 -> Bls12_381_g1
    | Bls12_381_g2 -> Bls12_381_g2
    | Bls12_381_fr -> Bls12_381_fr
    | Chest_key -> Chest_key
    | Chest -> Chest
  ;;
end

include Recursion.Fixpoint (T)
