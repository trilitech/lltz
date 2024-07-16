(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Michelson_base.Type
open Utils

type ty =
  |  T0 of type0
  |  T1 of type1 * ty
  |  T2 of type2 * ty * ty
  |  Record of row
  |  Variant of row
  |  Vector of ty list
  |  Missing of string
[@@deriving eq, show]

and row = (string option * ty) Binary_tree.t [@@deriving eq, show]

type tys =
  | Stack_ok of ty
  | Stack_failed
[@@deriving eq, show]

val lub : ty -> ty -> ty option

val lub_row : bool -> row -> row -> row option

val lubs : tys -> tys -> tys option

val compatible : ty -> ty -> bool

val t_unit : ty

val t_missing : string -> ty

val t_nat : ty

val t_int : ty

val t_mutez : ty

val t_string : ty

val t_bytes : ty

val t_chain_id : ty

val t_timestamp : ty

val t_address : ty

val t_key : ty

val t_key_hash : ty

val t_signature : ty

val t_operation : ty

val t_sapling_state : int -> ty

val t_sapling_transaction : int -> ty

val t_never : ty

val t_list : ty -> ty

val t_set : ty -> ty

val t_contract : ty -> ty

val t_ticket : ty -> ty

val t_map : ty -> ty -> ty

val t_big_map : ty -> ty -> ty

val t_lambda : ty -> ty -> ty

val t_variant : row -> ty

val t_record : row -> ty

val t_variant_node : row -> row -> ty

val t_record_node : row -> row -> ty

val leaf : ?lbl:string -> ty -> row

val t_bool : ty

val r_option : ty -> row

val t_option : ty -> ty

val t_pair : ?annot1:string -> ?annot2:string -> ty -> ty -> ty

val r_or : ty -> ty -> row

val t_or : ty -> ty -> ty

val print_row : sep:string -> Format.formatter -> row -> unit

val print_ty : Format.formatter -> ty -> unit

val print_tys : Format.formatter -> tys -> unit

val get1 : tys -> (ty, string) result

val unleaf_variant : row -> ty

val view_option : ty -> ty option

val view_or : ty -> (ty * ty) option

val t_bls12_381_g1 : ty

val t_bls12_381_g2 : ty

val t_bls12_381_fr : ty
