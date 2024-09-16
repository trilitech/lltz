(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

module Prefix : sig
  val block_hash : string

  val operation_hash : string

  val operation_list_hash : string

  val operation_list_list_hash : string

  val protocol_hash : string

  val context_hash : string

  val block_metadata_hash : string

  val operation_metadata_hash : string

  val operation_metadata_list_hash : string

  val operation_metadata_list_list_hash : string

  val ed25519_public_key_hash : string

  val secp256k1_public_key_hash : string

  val p256_public_key_hash : string

  val cryptobox_public_key_hash : string

  val ed25519_seed : string

  val ed25519_public_key : string

  val ed25519_secret_key : string

  val ed25519_signature : string

  val secp256k1_public_key : string

  val secp256k1_secret_key : string

  val secp256k1_signature : string

  val p256_public_key : string

  val p256_secret_key : string

  val p256_signature : string

  val ed25519_encrypted_seed : string

  val secp256k1_encrypted_secret_key : string

  val p256_encrypted_secret_key : string

  val generic_signature : string

  val chain_id : string

  val script_expr_hash : string

  val contract_hash : string

  val secp256k1_element : string

  val secp256k1_scalar : string

  val sapling_spending_key : string

  val sapling_address : string
end

val decode_secret_key : string -> string

val decode_key_hash : string -> string

val decode_address : string -> string

val decode_key : string -> string

val decode_signature : string -> string

val encode_by_prefix : prefix:string -> string -> string

val encode_key_hash : string -> string

val encode_address : string -> string

val encode_key : string -> string

val encode_signature : string -> string

val encode_hex : string -> string

val decode_to_hex : string -> string

val encode_expr : string -> string
