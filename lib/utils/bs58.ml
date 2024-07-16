(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

type types =
  | Key_Hash
  | Address
  | Key
  | Signature
  | Secret_Key
  | Script_Expr_Hash

(* https://gitlab.com/tezos/tezos/-/blob/master/src/lib_crypto/base58.ml *)
module Prefix = struct
  (* 32 *)
  let block_hash = "\001\052" (* B(51) *)

  let operation_hash = "\005\116" (* o(51) *)

  let operation_list_hash = "\133\233" (* Lo(52) *)

  let operation_list_list_hash = "\029\159\109" (* LLo(53) *)

  let protocol_hash = "\002\170" (* P(51) *)

  let context_hash = "\079\199" (* Co(52) *)

  let block_metadata_hash = "\234\249" (* bm(52) *)

  let operation_metadata_hash = "\005\183" (* r(51) *)

  let operation_metadata_list_hash = "\134\039" (* Lr(52) *)

  let operation_metadata_list_list_hash = "\029\159\182" (* LLr(53) *)

  (* 20 *)
  let ed25519_public_key_hash = "\006\161\159" (* tz1(36) *)

  let secp256k1_public_key_hash = "\006\161\161" (* tz2(36) *)

  let p256_public_key_hash = "\006\161\164" (* tz3(36) *)

  (* 16 *)
  let cryptobox_public_key_hash = "\153\103" (* id(30) *)

  (* 32 *)
  let ed25519_seed = "\013\015\058\007" (* edsk(54) *)

  let ed25519_public_key = "\013\015\037\217" (* edpk(54) *)

  let secp256k1_secret_key = "\017\162\224\201" (* spsk(54) *)

  let p256_secret_key = "\016\081\238\189" (* p2sk(54) *)

  (* 56 *)
  let ed25519_encrypted_seed = "\007\090\060\179\041" (* edesk(88) *)

  let secp256k1_encrypted_secret_key = "\009\237\241\174\150" (* spesk(88) *)

  let p256_encrypted_secret_key = "\009\048\057\115\171" (* p2esk(88) *)

  (* 33 *)
  let secp256k1_public_key = "\003\254\226\086" (* sppk(55) *)

  let p256_public_key = "\003\178\139\127" (* p2pk(55) *)

  let secp256k1_scalar = "\038\248\136" (* SSp(53) *)

  let secp256k1_element = "\005\092\000" (* GSp(54) *)

  (* 64 *)
  let ed25519_secret_key = "\043\246\078\007" (* edsk(98) *)

  let ed25519_signature = "\009\245\205\134\018" (* edsig(99) *)

  let secp256k1_signature = "\013\115\101\019\063" (* spsig1(99) *)

  let p256_signature = "\054\240\044\052" (* p2sig(98) *)

  let generic_signature = "\004\130\043" (* sig(96) *)

  (* 4 *)
  let chain_id = "\087\082\000" (* Net(15) *)

  let script_expr_hash =
    (* Taken from src/proto_006_PsCARTHA/lib_protocol/script_expr_hash.ml *)
    (* expr(54) *)
    "\013\044\064\027"

  let contract_hash =
    (* src/proto_006_PsCARTHA/lib_protocol/contract_hash.ml KT1(36) *)
    "\002\090\121"

  (*
    Added in protocol 8
  *)

  (* 169 *)
  let sapling_spending_key = "\011\237\020\092" (* sask(241) *)

  (* 43 *)
  let sapling_address = "\018\071\040\223"

  (* zet1(69) *)

  (*
     Added in protocol 9
  *)
end

let get_kt1_suffix data =
  match Stdlib.String.index_opt data '%' with
  | None -> (data, "00")
  | Some i ->
      let address = String.sub data 0 i in
      let suffix = String.sub data (i + 1) (String.length data - i - 1) in
      (address, "00" ^ Hex.(show (of_string suffix)))

let find_prefix data ~data_type ~prefix_length =
  match (data_type, String.sub data 0 prefix_length) with
  | Key_Hash, "tz1" -> ("00", data, Prefix.ed25519_public_key_hash, "")
  | Key_Hash, "tz2" -> ("01", data, Prefix.secp256k1_public_key_hash, "")
  | Key_Hash, "tz3" -> ("02", data, Prefix.p256_public_key_hash, "")
  | Address, "tz1" -> ("0000", data, Prefix.ed25519_public_key_hash, "")
  | Address, "tz2" -> ("0001", data, Prefix.secp256k1_public_key_hash, "")
  | Address, "tz3" -> ("0002", data, Prefix.p256_public_key_hash, "")
  | Address, "KT1" ->
      let address, suffix = get_kt1_suffix data in
      ("01", address, Prefix.contract_hash, suffix)
  | Key, "edpk" -> ("00", data, Prefix.ed25519_public_key, "")
  | Key, "sppk" -> ("01", data, Prefix.secp256k1_public_key, "")
  | Key, "p2pk" -> ("02", data, Prefix.p256_public_key, "")
  | Signature, "sig" -> ("", data, Prefix.generic_signature, "")
  | Signature, "edsig" -> ("", data, Prefix.ed25519_signature, "")
  | Signature, "spsig1" -> ("", data, Prefix.secp256k1_signature, "")
  | Signature, "p2sig" -> ("", data, Prefix.p256_signature, "")
  | Secret_Key, "ede" -> ("", data, Prefix.ed25519_encrypted_seed, "")
  | Secret_Key, "spe" -> ("", data, Prefix.secp256k1_encrypted_secret_key, "")
  | Secret_Key, "p2e" -> ("", data, Prefix.p256_encrypted_secret_key, "")
  | Secret_Key, "eds" ->
      let prefix =
        if String.length data = 32
        then Prefix.ed25519_seed
        else Prefix.ed25519_secret_key
      in
      ("", data, prefix, "")
  | Secret_Key, "spsk" -> ("", data, Prefix.secp256k1_secret_key, "")
  | Secret_Key, "p2s" -> ("", data, Prefix.p256_secret_key, "")
  | Script_Expr_Hash, "expr" -> ("", data, Prefix.script_expr_hash, "")
  | _ -> Fmt.failwith "[Decoding] Unknown prefix for data: %s" data

let find_prefix_reverse rawData ~data_type ~prefix_length =
  let prefix = String.sub rawData 0 prefix_length in
  let data =
    String.sub rawData prefix_length (String.length rawData - prefix_length)
  in
  match (data_type, prefix) with
  | Key_Hash, "00" -> (data, Prefix.ed25519_public_key_hash, "")
  | Key_Hash, "01" -> (data, Prefix.secp256k1_public_key_hash, "")
  | Key_Hash, "02" -> (data, Prefix.p256_public_key_hash, "")
  | Address, "00" -> (
      let prefix = String.sub data 0 2 in
      let data = String.sub data 2 (String.length data - 2) in
      match prefix with
      | "00" -> (data, Prefix.ed25519_public_key_hash, "")
      | "01" -> (data, Prefix.secp256k1_public_key_hash, "")
      | "02" -> (data, Prefix.p256_public_key_hash, "")
      | _ -> Fmt.failwith "Unknown address: %s" rawData)
  | Address, "01" ->
      let address = String.sub data 0 40 in
      let suffix =
        if String.length data > 42
        then
          "%"
          ^ Hex.to_string (`Hex (String.sub data 42 (String.length data - 42)))
        else ""
      in
      (address, Prefix.contract_hash, suffix)
  | Key, "00" -> (data, Prefix.ed25519_public_key, "")
  | Key, "01" -> (data, Prefix.secp256k1_public_key, "")
  | Key, "02" -> (data, Prefix.p256_public_key, "")
  | Signature, _ -> (data, Prefix.generic_signature, "")
  | Script_Expr_Hash, _ -> (data, Prefix.script_expr_hash, "")
  | _ -> Fmt.failwith "[Encoding] Unknown prefix for data : %s" rawData

let decode data ~data_type ~prefix_length =
  let prefix, data_to_decode, prefix_bytes, suffix =
    find_prefix data ~data_type ~prefix_length
  in
  let bytes = Base58.to_bytes_exn (`Base58 data_to_decode) in
  let hash = Base.String.chop_prefix bytes ~prefix:prefix_bytes in
  let (`Hex result) =
    match hash with
    | Some h -> Hex.of_string h
    | None -> Fmt.failwith "Could not decode data: %s" data
  in
  prefix ^ result ^ suffix

let encode data ~data_type ~prefix_length =
  let data_to_encode, prefix_bytes, suffix =
    find_prefix_reverse data ~data_type ~prefix_length
  in
  let bytes = prefix_bytes ^ Hex.to_string (`Hex data_to_encode) in
  let (`Base58 encoded) = Base58.of_bytes bytes in
  encoded ^ suffix

let decode_to_hex data =
  Hex.(show (of_string (Base58.to_bytes_exn (`Base58 data))))

let decode_secret_key data = decode data ~data_type:Secret_Key ~prefix_length:3

let decode_key_hash data = decode data ~data_type:Key_Hash ~prefix_length:3

let decode_address data = decode data ~data_type:Address ~prefix_length:3

let decode_key data = decode data ~data_type:Key ~prefix_length:4

let decode_signature data =
  match String.sub data 0 3 with
  | "sig" -> decode data ~data_type:Signature ~prefix_length:3
  | "sps" -> decode data ~data_type:Signature ~prefix_length:6 (* spsig1 *)
  | _ -> decode data ~data_type:Signature ~prefix_length:5

let encode_by_prefix ~prefix data =
  let bytes = prefix ^ Hex.to_string (`Hex data) in
  let (`Base58 encoded) = Base58.of_bytes bytes in
  encoded

let encode_hex data =
  let bytes = Hex.to_string (`Hex data) in
  let (`Base58 encoded) = Base58.of_bytes bytes in
  encoded

let encode_key_hash data = encode data ~data_type:Key_Hash ~prefix_length:2

let encode_address data = encode data ~data_type:Address ~prefix_length:2

let encode_key data = encode data ~data_type:Key ~prefix_length:2

let encode_signature data = encode data ~data_type:Signature ~prefix_length:0

let encode_expr data = encode data ~data_type:Script_Expr_Hash ~prefix_length:0

(*
 *  PACK
 *
 *  - octez-client --endpoint https://ghostnet.smartpy.io hash data '"KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat"' of type 'address'
 *  - octez-client --endpoint https://ghostnet.smartpy.io hash data '"tz1fextP23D6Ph2zeGTP8EwkP5Y8TufeFCHA"' of type 'key_hash'
 *  - octez-client --endpoint https://ghostnet.smartpy.io hash data '"edpktppVJVhoLCs27UwX9BFEPN4Q3BTiLpv8y4ipHUQmxPki17w79A"' of type 'key'
 *  - octez-client --endpoint https://ghostnet.smartpy.io hash data '"edsigthw6sSCfcZbjKCqGE9CZ9PoTsW1Kh5cgGu5SSU1AzcKyJ37oubeKVnDfY291minBiui7khzr8pzhoFVtF9ULs3hnUVKXGx"' of type 'signature'
 *  - octez-client --endpoint https://falphanet.smartpy.io hash data '"SG1jfZeHRzeWAM1T4zrwunEyUpwWc82D4tbv"' of type 'baker_hash'
 *
 *  UNPACK
 *
 *  - octez-client --endpoint https://ghostnet.smartpy.io unpack michelson data '0x050a000000404d6738931b59605ca0449b7b76a01d210ace9220051821ecf43382a7024ed99063c5bedb85c81b919d33a213c6d8fb47f2c9b4deaf6f68f56dee038ea739740f'
 *  - octez-client --endpoint https://ghostnet.smartpy.io normalize data '0x4d6738931b59605ca0449b7b76a01d210ace9220051821ecf43382a7024ed99063c5bedb85c81b919d33a213c6d8fb47f2c9b4deaf6f68f56dee038ea739740f' of type 'signature'
 *)
