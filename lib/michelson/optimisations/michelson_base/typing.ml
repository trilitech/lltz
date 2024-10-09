(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Type
open Printf
open Primitive

let unify_annots pref ?tolerant a b =
  match (a, b) with
  | Some a, Some b when a = b -> Ok (Some a)
  | Some a, Some b ->
      if tolerant = Some ()
      then Ok None
      else
        Error
          (sprintf "Cannot unify annotations '%s%s' and '%s%s'" pref a pref b)
  | _ -> Ok None

let rec unify_types ?tolerant t u =
  let open Utils.Control in
  let unify_types = unify_types ?tolerant in
  let mk {mt} =
    match
      ( unify_annots ?tolerant ":" t.annot_type u.annot_type
      , unify_annots ~tolerant:() "@" t.annot_variable u.annot_variable )
    with
    | Error e, _ | _, Error e -> Error e
    | Ok annot_type, Ok annot_variable -> Ok {mt; annot_type; annot_variable}
  in
  let open Result in
  let err () =
    Error
      (sprintf "Cannot unify types '%s' and '%s'." (show_mtype t) (show_mtype u))
  in
  match (t.mt, u.mt) with
  | MT0 t1, MT0 t2 -> if equal_type0 t1 t2 then mk t else err ()
  | MT1 (Option, t), MT1 (Option, u) -> mt_option <$> unify_types t u
  | MT1 (List, t), MT1 (List, u) -> mt_list <$> unify_types t u
  | MT1 (Ticket, t), MT1 (Ticket, u) -> mt_ticket <$> unify_types t u
  | MT1 (Set, t), MT1 (Set, u) -> mt_set <$> unify_types t u
  | MT1 (Contract, t), MT1 (Contract, u) -> mt_contract <$> unify_types t u
  | ( MT2 (Pair {annot_fst = a1; annot_snd = a2}, t1, t2)
    , MT2 (Pair {annot_fst = b1; annot_snd = b2}, u1, u2) ) ->
      let* annot_fst = unify_annots ?tolerant "%" a1 b1 in
      let* v1 = unify_types t1 u1 in
      let* annot_snd = unify_annots ?tolerant "%" a2 b2 in
      let* v2 = unify_types t2 u2 in
      mk (mt_pair ?annot_fst v1 ?annot_snd v2)
  | ( MT2 (Or {annot_left = a1; annot_right = a2}, t1, t2)
    , MT2 (Or {annot_left = b1; annot_right = b2}, u1, u2) ) ->
      let* annot_left = unify_annots ?tolerant "%" a1 b1 in
      let* v1 = unify_types t1 u1 in
      let* annot_right = unify_annots ?tolerant "%" a2 b2 in
      let* v2 = unify_types t2 u2 in
      mk (mt_or ?annot_left v1 ?annot_right v2)
  | MT2 (Lambda, t1, t2), MT2 (Lambda, u1, u2) ->
      mt_lambda <$> unify_types t1 u1 <*> unify_types t2 u2 >>= mk
  | MT2 (Map, t1, t2), MT2 (Map, u1, u2) ->
      mt_map <$> unify_types t1 u1 <*> unify_types t2 u2 >>= mk
  | MT2 (Big_map, t1, t2), MT2 (Big_map, u1, u2) ->
      mt_big_map <$> unify_types t1 u1 <*> unify_types t2 u2 >>= mk
  | MT_var t_name, MT_var u_name when t_name = u_name -> mk (mt_var t_name)
  | _ -> err ()

let type_prim0 = function
  | Now -> {mt_timestamp with annot_variable = Some "now"}
  | Chain_id -> mt_chain_id
  | Sender -> {mt_address with annot_variable = Some "sender"}
  | Source -> {mt_address with annot_variable = Some "source"}
  | Amount -> {mt_mutez with annot_variable = Some "amount"}
  | Balance -> {mt_mutez with annot_variable = Some "balance"}
  | Total_voting_power -> mt_nat
  | Self_address -> {mt_address with annot_variable = Some "self"}
  | Sapling_empty_state {memo} -> mt_sapling_state memo
  | Level -> {mt_nat with annot_variable = Some "level"}
  | Self _ -> assert false
  | Empty_bigmap _ -> assert false
  | Empty_map _ -> assert false
  | Empty_set _ -> assert false
  | Nil _ -> assert false
  | None_ _ -> assert false
  | Unit_ -> assert false

let type_prim1 = function
  | IsNat -> (mt_int, mt_option mt_nat)
  | Abs -> (mt_int, mt_nat)
  | Not -> assert false
  | Hash_key -> (mt_key, mt_key_hash)
  | Blake2b -> (mt_bytes, mt_bytes)
  | Sha256 -> (mt_bytes, mt_bytes)
  | Sha512 -> (mt_bytes, mt_bytes)
  | Keccak -> (mt_bytes, mt_bytes)
  | Sha3 -> (mt_bytes, mt_bytes)
  | Car
  | Cdr
  | Some_
  | Eq
  | Neg
  | Nat
  | Int
  | Bytes
  | Neq
  | Le
  | Lt
  | Ge
  | Gt
  | Concat1
  | Size
  | Address
  | Implicit_account
  | Pack
  | Set_delegate
  | Read_ticket
  | Join_tickets
  | Pairing_check
  | Voting_power
  | Left (_, _, _)
  | Right (_, _, _)
  | Contract (_, _)
  | Unpack _ | Getn _ | Cast _ | Rename _
  | Emit (_, _) -> assert false

let type_prim2 = function
  | Sub_mutez -> [((mt_mutez, mt_mutez), mt_option mt_mutez)]
  | Add ->
      [
        ((mt_nat, mt_nat), mt_nat)
      ; ((mt_nat, mt_int), mt_int)
      ; ((mt_int, mt_nat), mt_int)
      ; ((mt_int, mt_int), mt_int)
      ; ((mt_timestamp, mt_int), mt_timestamp)
      ; ((mt_int, mt_timestamp), mt_timestamp)
      ; ((mt_mutez, mt_mutez), mt_mutez)
      ; ((mt_bls12_381_g1, mt_bls12_381_g1), mt_bls12_381_g1)
      ; ((mt_bls12_381_g2, mt_bls12_381_g2), mt_bls12_381_g2)
      ; ((mt_bls12_381_fr, mt_bls12_381_fr), mt_bls12_381_fr)
      ]
  | Mul ->
      [
        ((mt_nat, mt_nat), mt_nat)
      ; ((mt_nat, mt_int), mt_int)
      ; ((mt_int, mt_nat), mt_int)
      ; ((mt_int, mt_int), mt_int)
      ; ((mt_mutez, mt_nat), mt_mutez)
      ; ((mt_nat, mt_mutez), mt_mutez)
      ; ((mt_bls12_381_g1, mt_bls12_381_fr), mt_bls12_381_g1)
      ; ((mt_bls12_381_g2, mt_bls12_381_fr), mt_bls12_381_g2)
      ; ((mt_bls12_381_fr, mt_bls12_381_fr), mt_bls12_381_fr)
      ; ((mt_nat, mt_bls12_381_fr), mt_bls12_381_fr)
      ; ((mt_int, mt_bls12_381_fr), mt_bls12_381_fr)
      ; ((mt_bls12_381_fr, mt_nat), mt_bls12_381_fr)
      ; ((mt_bls12_381_fr, mt_int), mt_bls12_381_fr)
      ]
  | Lsl
  | Lsr
  | Sub
  | Xor
  | Ediv
  | And
  | Or
  | Cons
  | Compare
  | Concat2
  | Get
  | Mem
  | Exec
  | Apply
  | Sapling_verify_update
  | Ticket
  | Ticket_deprecated
  | Split_ticket
  | Pair _
  | Updaten _
  | View _ -> assert false

let type_prim3 = function
  | Check_signature -> (mt_key, mt_signature, mt_bytes, mt_bool)
  | Open_chest -> (mt_chest_key, mt_chest, mt_nat, mt_or mt_bytes mt_bool)
  | Slice | Update | Get_and_update | Transfer_tokens -> assert false
