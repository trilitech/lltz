(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Sexplib.Std

type 'ty prim0 =
  | Amount
  | Balance
  | Chain_id
  | Level
  | Now
  | Self of string option
  | Self_address
  | Sender
  | Source
  | Total_voting_power
  | Empty_bigmap of 'ty * 'ty
  | Empty_map of 'ty * 'ty
  | Empty_set of 'ty
  | Nil of 'ty
  | None_ of 'ty
  | Sapling_empty_state of {memo : int}
  | Unit_
[@@deriving eq, ord, show {with_path = false}, map, fold, sexp]

type 'ty prim1 =
  | Car
  | Cdr
  | Left of string option * string option * 'ty
  | Right of string option * string option * 'ty
  | Some_
  | Eq
  | Abs
  | Neg
  | Nat
  | Int
  | Bytes
  | IsNat
  | Neq
  | Le
  | Lt
  | Ge
  | Gt
  | Not
  | Concat1
  | Size
  | Address
  | Implicit_account
  | Contract of string option * 'ty
  | Pack
  | Unpack of 'ty
  | Hash_key
  | Blake2b
  | Sha256
  | Sha512
  | Keccak
  | Sha3
  | Set_delegate
  | Read_ticket
  | Join_tickets
  | Pairing_check
  | Voting_power
  | Getn of int
  | Cast of 'ty
  | Rename of string option
  | Emit of string option * 'ty option
[@@deriving eq, ord, show {with_path = false}, map, sexp]

type prim1_fail =
  | Failwith
  | Never
[@@deriving eq, ord, show {with_path = false}, sexp]

type 'ty prim2 =
  | Pair of string option * string option
  | Add
  | Mul
  | Sub
  | Sub_mutez
  | Lsr
  | Lsl
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
  | Updaten of int
  | View of string (* view name *) * 'ty (* return type *)
[@@deriving eq, ord, show {with_path = false}, map, sexp]

type prim3 =
  | Slice
  | Update
  | Get_and_update
  | Transfer_tokens
  | Check_signature
  | Open_chest
[@@deriving eq, ord, show {with_path = false}, sexp]
