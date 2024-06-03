(*  constructors taken from smartpy but conflated into a single structure *)
type 'ty t =
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
  | Empty_bigmap of 'ty * 'ty
  | Empty_map of 'ty * 'ty
  | Empty_set of 'ty
  | Nil of 'ty
  | None_ of 'ty
  | Sapling_empty_state of { memo : int }
  | Unit_
  (* arity 1 *)
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
  (* arity 1 (can fail)*)
  | Failwith
  | Never
  (* arity 2 *)
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
  (* arity 3 *)
  | Slice
  | Update
  | Get_and_update
  | Transfer_tokens
  | Check_signature
  | Open_chest
