(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

(** Michelson Nullary Primitives *)
type 'ty prim0 =
  | Amount (** The amount of the transaction. *)
  | Balance (** The balance of the current contract.*)
  | Chain_id (** The current chain_id.*)
  | Level
  | Now (** The timestamp of the current block.*)
  | Self of string option (** The current contract.*)
  | Self_address (**The current contract address.*)
  | Sender (** The sender of the current transaction.*)
  | Source (** The initial source of the transactions that triggered the*)
  | Total_voting_power (** The total voting power in the current block.*)
  | Empty_bigmap of 'ty * 'ty (** Push an Empty Bigmap.*)
  | Empty_map of 'ty * 'ty (** Push an Empty Map.*)
  | Empty_set of 'ty (** Push an Empty Set.*)
  | Nil of 'ty (** Push an Empty List.*)
  | None_ of 'ty (** Push None.*)
  | Sapling_empty_state of { memo : int } (** Push an Empty Sapling State.*)
  | Unit_ (** Push unit.*)
  | Min_block_time (** Push the current minimal block time in seconds. *)
[@@deriving eq, ord, show, map, fold, sexp_of]

(** Michelson Unary Primitives *)
type 'ty prim1 =
  | Car (** Left projection.*)
  | Cdr (** Right projection.*)
  | Left of string option * string option * 'ty (** Left constructor.*)
  | Right of string option * string option * 'ty (** Right constructor.*)
  | Some_ (** Some constructor.*)
  | Eq (** Equality.*)
  | Abs (** Absolute value.*)
  | Neg (** Negation.*)
  | Nat
  | Int (** Conversion of nat to int.*)
  | Bytes
  | IsNat (** Conversion of int to nat option.*)
  | Neq (** Non-equality.*)
  | Le (** Lower or Equal.*)
  | Lt (** Lower Than.*)
  | Ge (** Greater or Equal.*)
  | Gt (** Greater Than.*)
  | Not (** Not.*)
  | Concat1 (** Concatenation of a list.*)
  | Size (** Size / Length.*)
  | Address (** Address of a contract.*)
  | Implicit_account (** Implicit Account of a key_hash.*)
  | Is_implicit_account (** Is_implicit Account of an address.*)
  | Contract of string option * 'ty (** Contract of an address and entrypoint.*)
  | Pack (** Packing values.*)
  | Unpack of 'ty (** Unpacking values.*)
  | Hash_key (** Conversion of a key into a key_hash. *)
  | Blake2b (** Blake2b hash.*)
  | Sha256 (** SHA256 hash.*)
  | Sha512 (** SHA512 hash.*)
  | Keccak (** KECCAK hash.*)
  | Sha3 (** SHA3 hash.*)
  | Set_delegate (**Set or withdraw the contract’s delegation. *)
  | Read_ticket (** Retrieve the information stored in a ticket. *)
  | Join_tickets (** Merge ticket amounts. *)
  | Pairing_check (** Pairing verification for BLS12_381.*)
  | Voting_power (** Account voting power. *)
  | Getn of int (** Comb projection. *)
  | Cast of 'ty (** Change toplevel type. *)
  | Rename of string option (** Change toplevel annotation. *)
  | Emit of string option * 'ty option
[@@deriving eq, ord, show, map, sexp_of]

(** Branch Terminating and Failing Primitives *)
type prim1_fail =
  | Failwith
  | Never
[@@deriving eq, ord, show, sexp_of]

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
[@@deriving eq, ord, show, map, sexp_of]

type prim3 =
  | Slice
  | Update
  | Get_and_update
  | Transfer_tokens
  | Check_signature
  | Open_chest
[@@deriving eq, ord, show, sexp_of]
