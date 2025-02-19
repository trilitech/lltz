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

  let rec is_duppable (t:t) =
    match t.desc with
    | Ticket _ -> false
    | Tuple row | Or row ->
      is_duppable_row row
    | Option t1 | List t1 | Set t1 ->
      is_duppable t1
    | Map (t1, t2) | Big_map (t1, t2) ->
      is_duppable t1 && is_duppable t2
    (* For primitive types and types without subtypes *)
    | _ -> true
    
  and is_duppable_row row =
    match row with
    | Node ts ->
        List.for_all ts ~f:is_duppable_row
    | Leaf (_, x) ->
        is_duppable x

  (* Check equality of types without considering ranges and optimization annotations *)
  and equal_types t1 t2 = 
    match t1.desc, t2.desc with
    | Tuple row1, Tuple row2
    | Or row1, Or row2 ->
      Row.equal_types equal_types row1 row2
    | Option t1, Option t2
    | List t1, List t2
    | Set t1, Set t2
    | Contract t1, Contract t2
    | Ticket t1, Ticket t2 ->
      equal_types t1 t2
    | Function (t1, t2), Function (t1', t2')
    | Map (t1, t2), Map (t1', t2')
    | Big_map (t1, t2), Big_map (t1', t2') ->
      equal_types t1 t1' && equal_types t2 t2'
    | Unit, Unit
    | Bool, Bool
    | Nat, Nat
    | Int, Int
    | Mutez, Mutez
    | String, String
    | Bytes, Bytes
    | Chain_id, Chain_id
    | Timestamp, Timestamp
    | Address, Address
    | Keys, Keys
    | Key_hash, Key_hash
    | Signature, Signature
    | Operation, Operation ->
      true
    | Sapling_state {memo = n1}, Sapling_state {memo = n2} -> n1 = n2
    | Sapling_transaction {memo = n1}, Sapling_transaction {memo = n2} -> n1 = n2
    | Never, Never
    | Bls12_381_g1, Bls12_381_g1
    | Bls12_381_g2, Bls12_381_g2
    | Bls12_381_fr, Bls12_381_fr
    | Chest_key, Chest_key
    | Chest, Chest
    | Tx_rollup_l2_address, Tx_rollup_l2_address ->
      true
    | _ -> false
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
