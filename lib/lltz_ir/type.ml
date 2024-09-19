open Import
open Grace

module T = struct
  type t =
    { desc : desc
    ; range : Range.t
    }

  and desc =
    (* sum and product types *)
    | Tuple of t Row.t
    | Or of t Row.t
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
    | Keys
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
    | Tx_rollup_l2_address
  [@@deriving sexp, equal, compare, traverse]
end

include T

module Traverse = struct
  class map =
    object
      inherit Traverse_builtins.map
      inherit Row.Traverse_builtins.map
      inherit T.map
      method range__t = Traverse_builtins.map_zero
    end

  class iter =
    object
      inherit Traverse_builtins.iter
      inherit Row.Traverse_builtins.iter
      inherit T.iter
      method range__t = Traverse_builtins.iter_zero
    end

  class ['acc] fold =
    object
      inherit ['acc] Traverse_builtins.fold
      inherit ['acc] Row.Traverse_builtins.fold
      inherit ['acc] T.fold
      method range__t = Traverse_builtins.fold_zero
    end

  class ['acc] fold_map =
    object
      inherit ['acc] Traverse_builtins.fold_map
      inherit ['acc] Row.Traverse_builtins.fold_map
      inherit ['acc] T.fold_map
      method range__t = Traverse_builtins.fold_map_zero
    end

  class ['ctx] map_with_context =
    object
      inherit ['ctx] Traverse_builtins.map_with_context
      inherit ['ctx] Row.Traverse_builtins.map_with_context
      inherit ['ctx] T.map_with_context
      method range__t = Traverse_builtins.map_with_context_zero
    end
end
