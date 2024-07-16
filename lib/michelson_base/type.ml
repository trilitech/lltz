(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Sexplib.Std

type type0 =
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
  | Sapling_state of {memo : int}
  | Sapling_transaction of {memo : int}
  | Never
  | Bls12_381_g1
  | Bls12_381_g2
  | Bls12_381_fr
  | Chest_key
  | Chest
[@@deriving eq, ord, show {with_path = false}, sexp]

type type1 =
  | Option
  | List
  | Set
  | Contract
  | Ticket
[@@deriving eq, ord, show {with_path = false}, sexp]

type type2 =
  | Lambda
  | Map
  | Big_map
  | Pair of {
        annot_fst : string option
      ; annot_snd : string option
    }
  | Or of {
        annot_left : string option
      ; annot_right : string option
    }
[@@deriving eq, ord, show {with_path = false}, sexp]

let string_of_type0 = function
  | Unit -> ("unit", None)
  | Bool -> ("bool", None)
  | Nat -> ("nat", None)
  | Int -> ("int", None)
  | Mutez -> ("mutez", None)
  | String -> ("string", None)
  | Bytes -> ("bytes", None)
  | Chain_id -> ("chain_id", None)
  | Timestamp -> ("timestamp", None)
  | Address -> ("address", None)
  | Key -> ("key", None)
  | Key_hash -> ("key_hash", None)
  | Signature -> ("signature", None)
  | Operation -> ("operation", None)
  | Sapling_state {memo} -> ("sapling_state", Some (string_of_int memo))
  | Sapling_transaction {memo} ->
      ("sapling_transaction", Some (string_of_int memo))
  | Never -> ("never", None)
  | Bls12_381_g1 -> ("bls12_381_g1", None)
  | Bls12_381_g2 -> ("bls12_381_g2", None)
  | Bls12_381_fr -> ("bls12_381_fr", None)
  | Chest_key -> ("chest_key", None)
  | Chest -> ("chest", None)

let string_of_type1 = function
  | Option -> "option"
  | List -> "list"
  | Set -> "set"
  | Contract -> "contract"
  | Ticket -> "ticket"

let string_of_type2 = function
  | Lambda -> ("lambda", None, None)
  | Map -> ("map", None, None)
  | Big_map -> ("big_map", None, None)
  | Pair {annot_fst; annot_snd} -> ("pair", annot_fst, annot_snd)
  | Or {annot_left; annot_right} -> ("or", annot_left, annot_right)

type 'm mtype_f =
  | MT0 of type0
  | MT1 of type1 * 'm
  | MT2 of type2 * 'm * 'm
  | MT_var of string
[@@deriving eq, ord, fold, map, show {with_path = false}, sexp]

type mtype = {
    mt : mtype mtype_f
  ; annot_type : string option (* :a *)
  ; annot_variable : string option (* @a *)
}
[@@deriving eq, ord, show {with_path = false}, sexp]

let mk_mtype ?annot_type ?annot_variable mt = {mt; annot_type; annot_variable}

let rec cata_mtype f {mt; annot_type; annot_variable} =
  f ?annot_type ?annot_variable (map_mtype_f (cata_mtype f) mt)

let mt0 t = mk_mtype (MT0 t)

let mt1 t t1 = mk_mtype (MT1 (t, t1))

let mt2 t t1 t2 = mk_mtype (MT2 (t, t1, t2))

let mt_unit = mt0 Unit

let mt_bool = mt0 Bool

let mt_nat = mt0 Nat

let mt_int = mt0 Int

let mt_mutez = mt0 Mutez

let mt_string = mt0 String

let mt_bytes = mt0 Bytes

let mt_chain_id = mt0 Chain_id

let mt_timestamp = mt0 Timestamp

let mt_address = mt0 Address

let mt_key = mt0 Key

let mt_key_hash = mt0 Key_hash

let mt_signature = mt0 Signature

let mt_operation = mt0 Operation

let mt_sapling_state memo = mt0 (Sapling_state {memo})

let mt_sapling_transaction memo = mt0 (Sapling_transaction {memo})

let mt_never = mt0 Never

let mt_bls12_381_g1 = mt0 Bls12_381_g1

let mt_bls12_381_g2 = mt0 Bls12_381_g2

let mt_bls12_381_fr = mt0 Bls12_381_fr

let mt_chest_key = mt0 Chest_key

let mt_chest = mt0 Chest

let mt_option = mt1 Option

let mt_list = mt1 List

let mt_set = mt1 Set

let mt_contract = mt1 Contract

let mt_ticket = mt1 Ticket

let mt_lambda t1 t2 = mk_mtype (MT2 (Lambda, t1, t2))

let mt_map t1 t2 = mk_mtype (MT2 (Map, t1, t2))

let mt_big_map t1 t2 = mk_mtype (MT2 (Big_map, t1, t2))

let mt_pair ?annot_fst ?annot_snd = mt2 (Pair {annot_fst; annot_snd})

let mt_or ?annot_left ?annot_right = mt2 (Or {annot_left; annot_right})

let mt_var s = mk_mtype (MT_var s)
