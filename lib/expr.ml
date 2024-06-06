open Common

module Base = struct
  type type_ = location Type.t
  type 'a row = 'a Type.row
  type row_path = Type.row_path
  type prim = type_ Primitive.t
  type var = Var of string
  type mut_var = Mut_var of string

  type const =
    | Unit
    | Bool of bool
    | Nat of Z.t
    | Int of Z.t
    | Mutez of Z.t
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

  type 'expr contract = { parameter : type_; storage : type_; body : 'expr }
  [@@deriving map]

  type micheline = (location, string) Tezos_micheline.Micheline.node list

  let map_row = Rose_tree.map

  type 'expr t =
    (* basic lambda calculus w/ primivatives + constants *)
    | Var of var
    | Let_in of var * 'expr * 'expr
    | Lambda of var * type_ * 'expr
    | Lambda_rec of var * var * type_ * 'expr
    | App of 'expr * 'expr
    | Const of const
    | Prim of prim * 'expr list
    (* mutability *)
    | Let_mut_in of mut_var * 'expr * 'expr
    | Deref of mut_var
    | Assign of mut_var * 'expr
    (* low-level control flow (conditional) *)
    | If_bool of 'expr * 'expr * 'expr
    | If_none of 'expr * 'expr * (var * 'expr)
    | If_cons of 'expr * 'expr * (var * var * 'expr)
    | If_left of 'expr * (var * 'expr) * (var * 'expr)
    (* low-level control flow (iterative) *)
    | While of 'expr * 'expr
    | While_left of 'expr * 'expr
    | For of mut_var * 'expr * 'expr * 'expr * 'expr
    | For_each of var list * 'expr * 'expr
    (* high-level control flow (iterative) *)
    | Map of 'expr * (var list * 'expr)
    | Fold_left of 'expr * 'expr * (var * 'expr)
    | Fold_right of 'expr * 'expr * (var * 'expr)
    (* tuples *)
    | Let_tuple_in of var list * 'expr * 'expr
    | Tuple of 'expr row
    | Proj of 'expr * row_path
    | Update of 'expr * row_path * 'expr
    (* sums *)
    | Inj of row_path * 'expr
    | Match of 'expr * 'expr row
    (* tezos specific *)
    | Raw_michelson of micheline
    | Create_contract of 'expr contract * 'expr * 'expr * 'expr
  [@@deriving map]
end

include Recursion.Fixpoint (Base)
