(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Utils
open Type

include module type of Michelson_base.Primitive

type lit =
  | Unit
  | Bool of bool
  | Nat of Bigint.t
  | Int of Bigint.t
  | Mutez of Bigint.t
  | String of string
  | Key of string
  | Key_hash of string
  | Bytes of string
  | Chain_id of string
  | Address of string
  | Timestamp of string
  | Bls12_381_g1 of string
  | Bls12_381_g2 of string
  | Bls12_381_fr of string
  | Signature of string
[@@deriving eq, show {with_path = false}]

type stack_op =
  | Swap
  | Dup of int
  | Dig of int
  | Dug of int
  | Drop of int
[@@deriving eq, show]

type row_context = (string option * ty) Binary_tree.context
[@@deriving eq, show]

type record_pattern = string option Binary_tree.t [@@deriving eq, show]

type pattern =
  | P_var of string option
  | P_vector of string option list
[@@deriving eq, show {with_path = false}]

type 'e match_clause = {
    cons : string option
  ; var : string option
  ; rhs : 'e
}
[@@deriving map, fold, show]

type 'e precontract = {
    tparameter : ty * string option
  ; tstorage : ty
  ; parameter_and_storage : string option
  ; body : 'e
}
[@@deriving eq, map, fold, show]

type 'e expr_f =
  | Var of string
  | Let_in of pattern * 'e * 'e
  | Lambda of string option * ty * ty * 'e
  | Lambda_rec of string option * string option * ty * ty * 'e
  | Lit of lit
  | Prim0 of ty prim0
  | Prim1 of ty prim1 * 'e
  | Prim1_fail of prim1_fail * 'e
  | Prim2 of ty prim2 * 'e * 'e
  | Prim3 of prim3 * 'e * 'e * 'e
  | Proj_field of string * 'e
  | Stack_op of stack_op * 'e list
  | Record of (string option * 'e) Binary_tree.t
  | Variant of string option * row_context * 'e
  | List of ty * 'e list
  | Set of ty * 'e list
  | Map of ty * ty * ('e * 'e) list
  | Match_record of record_pattern * 'e * 'e
  | Match_variant of 'e * 'e match_clause Binary_tree.t
  | Vector of 'e list
  | Nth of int * 'e
  | Unpair of int * 'e
  | If of 'e * 'e * 'e
  | If_none of 'e * string option * 'e * 'e
  | If_left of 'e * string option * 'e * string option * 'e
  | If_cons of 'e * string option * string option * 'e * 'e
  | Loop of 'e list * string option list * 'e
  | Iter_over of 'e list * string option list * 'e
  | Map_over of 'e list * string option list * 'e
  | Create_contract of 'e precontract * 'e * 'e * 'e
  | Record_of_tree of string option Binary_tree.t * 'e
  | Comment of string list * 'e
[@@deriving map, fold, show]

(*module Traversable (A : APPLICATIVE) : sig
  module BT : module type of Binary_tree.Traversable (A)

  val sequenceA : 'a A.t expr_f -> 'a expr_f A.t
end*)

type texpr = {
    texpr : texpr expr_f
  ; tys : tys
}
[@@deriving eq, show]

type expr = {expr : expr expr_f} [@@deriving eq, show]

type env = (string * ty) list

val print_precontract :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a precontract -> unit

val print_record_pattern : Format.formatter -> record_pattern -> unit

val print_pattern : Format.formatter -> pattern -> unit

val print_vars : Format.formatter -> string option list -> unit

val record : (string option * expr) Binary_tree.t -> expr

val tuple : expr list -> expr

val proj_field : string -> expr -> expr

val match_record : string option Utils.Binary_tree.t -> expr -> expr -> expr

val match_variant : expr -> expr match_clause Binary_tree.t -> expr

val loop : expr list -> string option list -> expr -> expr

val prim0 : ty prim0 -> expr

val prim1 : ty prim1 -> expr -> expr

val prim1_fail : prim1_fail -> expr -> expr

val prim2 : ty prim2 -> expr -> expr -> expr

val prim3 : prim3 -> expr -> expr -> expr -> expr

val lit : lit -> expr

val var : string -> expr

val michel_list : ty -> expr list -> expr

val michel_set : ty -> expr list -> expr

val michel_map : ty -> ty -> (expr * expr) list -> expr

val variant : string option -> row_context -> expr -> expr

val unit : expr

val row_context : string -> row -> row_context option

val let_in : pattern -> expr -> expr -> expr

val let_in_var : string option -> expr -> expr -> expr

val let_in_vector : string option list -> expr -> expr -> expr

val lets : (pattern * expr) list -> expr -> expr

val lambda : string option -> ty -> ty -> expr -> expr

val lambda_rec : string option -> string option -> ty -> ty -> expr -> expr

val some : expr -> expr

val none : ty -> expr

val left : string option -> string option -> ty -> expr -> expr

val right : string option -> string option -> ty -> expr -> expr

val nil : ty -> expr

val true_ : expr

val false_ : expr

val dup : int -> expr list -> expr

val vector : expr list -> expr

val nth : int -> expr -> expr

val if_ : expr -> expr -> expr -> expr

val if_none : expr -> expr -> string option * expr -> expr

val if_left : expr -> string option * expr -> string option * expr -> expr

val if_cons : expr -> string option * string option * expr -> expr -> expr

val pair : string option -> string option -> expr -> expr -> expr

val stack_op : stack_op -> expr list -> expr

val create_contract : expr precontract -> expr -> expr -> expr -> expr

val record_of_tree : string option Utils.Binary_tree.t -> expr -> expr

val comment : string list -> expr -> expr

val map_over : expr list -> string option list -> expr -> expr

val iter_over : expr list -> string option list -> expr -> expr

val unroll_lets : expr -> (pattern * expr) list * expr

val unpair : int -> expr -> expr
