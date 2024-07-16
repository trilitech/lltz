open Import

(*  constructors taken from smartpy but conflated into a single structure *)

type t =
  (* arity 0 *)
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
  | Empty_bigmap of Type.t * Type.t
  | Empty_map of Type.t * Type.t
  | Empty_set of Type.t
  | Nil of Type.t
  | None of Type.t
  | Sapling_empty_state of { memo : int }
  | Unit
  (* arity 1 *)
  | Car
  | Cdr
  | Left of string option * string option * Type.t
  | Right of string option * string option * Type.t
  | Some
  | Eq
  | Abs
  | Neg
  | Nat
  | Int
  | Bytes
  | Is_nat
  | Neq
  | Le
  | Lt
  | Ge
  | Gt
  | Not
  | Size
  | Address
  | Implicit_account
  | Contract of string option * Type.t
  | Pack
  | Unpack of Type.t
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
  | Cast of Type.t
  | Rename of string option
  | Emit of string option * Type.t option
  | Failwith
  | Never
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
  | Concat1
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
  | View of string (* view name *) * Type.t (* return type *)
  (* arity 3 *)
  | Slice
  | Update
  | Get_and_update
  | Transfer_tokens
  | Check_signature
  | Open_chest
[@@deriving sexp, equal, compare]
