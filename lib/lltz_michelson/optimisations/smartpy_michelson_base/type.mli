(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

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
  | Sapling_state of { memo : int }
  | Sapling_transaction of { memo : int }
  | Never
  | Bls12_381_g1
  | Bls12_381_g2
  | Bls12_381_fr
  | Chest_key
  | Chest
[@@deriving eq, ord, show, sexp_of]

type type1 =
  | Option
  | List
  | Set
  | Contract
  | Ticket
[@@deriving eq, ord, show, sexp_of]

type type2 =
  | Lambda
  | Map
  | Big_map
  | Pair of
      { annot_fst : string option
      ; annot_snd : string option
      }
  | Or of
      { annot_left : string option
      ; annot_right : string option
      }
[@@deriving eq, ord, show, sexp_of]

val string_of_type0 : type0 -> string * string option
val string_of_type1 : type1 -> string
val string_of_type2 : type2 -> string * string option * string option

type 'm mtype_f =
  | MT0 of type0
  | MT1 of type1 * 'm
  | MT2 of type2 * 'm * 'm
  | MT_var of string
[@@deriving eq, ord, show, map, fold]

type mtype =
  { mt : mtype mtype_f
  ; annot_type : string option (* :a *)
  ; annot_variable : string option (* @a *)
  }
[@@deriving eq, ord, show, sexp_of]

val cata_mtype
  :  (?annot_type:string -> ?annot_variable:string -> 'a mtype_f -> 'a)
  -> mtype
  -> 'a

val mk_mtype : ?annot_type:string -> ?annot_variable:string -> mtype mtype_f -> mtype
val mt0 : type0 -> mtype
val mt1 : type1 -> mtype -> mtype
val mt2 : type2 -> mtype -> mtype -> mtype
val mt_unit : mtype
val mt_bool : mtype
val mt_nat : mtype
val mt_int : mtype
val mt_mutez : mtype
val mt_string : mtype
val mt_bytes : mtype
val mt_chain_id : mtype
val mt_timestamp : mtype
val mt_address : mtype
val mt_key : mtype
val mt_key_hash : mtype
val mt_signature : mtype
val mt_operation : mtype
val mt_sapling_state : int -> mtype
val mt_sapling_transaction : int -> mtype
val mt_never : mtype
val mt_bls12_381_g1 : mtype
val mt_bls12_381_g2 : mtype
val mt_bls12_381_fr : mtype
val mt_chest_key : mtype
val mt_chest : mtype
val mt_option : mtype -> mtype
val mt_list : mtype -> mtype
val mt_set : mtype -> mtype
val mt_contract : mtype -> mtype
val mt_ticket : mtype -> mtype
val mt_lambda : mtype -> mtype -> mtype
val mt_map : mtype -> mtype -> mtype
val mt_big_map : mtype -> mtype -> mtype
val mt_pair : ?annot_fst:string -> ?annot_snd:string -> mtype -> mtype -> mtype
val mt_or : ?annot_left:string -> ?annot_right:string -> mtype -> mtype -> mtype
val mt_var : string -> mtype
