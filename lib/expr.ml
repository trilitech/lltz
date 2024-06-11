open Common

module Base = struct
  type type_ = location Type.t
  type 'a row = 'a Type.row
  type row_path = Type.row_path
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

  type 'expr contract = { parameter : type_; storage : type_; code : 'expr }
  [@@deriving map]

  type micheline = (location, string) Tezos_micheline.Micheline.node list

  let map_row = Rose_tree.map

  type 'expr t =
    (* basic lambda calculus w/ primitives + constants *)
    | Var of var
    | Let_in of { let_var : var; rhs : 'expr; in_ : 'expr }
    | Lambda of { lam_var : var; return : type_; body : 'expr }
    | Lambda_rec of {
        lam_var : var
      ; mu_var : var
      ; return : type_
      ; body : 'expr }
    | App of { abs : 'expr; arg : 'expr }
    | Const of const
    | Prim0 of type_ Primitive.t0
    | Prim1 of type_ Primitive.t1 * 'expr
    | Prim2 of type_ Primitive.t2 * 'expr * 'expr
    | Prim3 of type_ Primitive.t3 * 'expr * 'expr * 'expr
    (* mutability *)
    | Let_mut_in of { let_var : mut_var; rhs : 'expr; in_ : 'expr }
    | Deref of mut_var
    | Assign of mut_var * 'expr
    (* low-level control flow (conditional) *)
    | If_bool of { condition : 'expr; if_true : 'expr; if_false : 'expr }
    | If_none of { subject : 'expr; if_none : 'expr; if_some : var * 'expr }
    | If_cons of {
          subject : 'expr
        ; if_empty : 'expr
        ; if_nonempty : var * var * 'expr
      }
    | If_left of {
          subject : 'expr
        ; if_left : var * 'expr
        ; if_right : var * 'expr
      }
    (* low-level control flow (iterative) *)
    | While of { invariant : 'expr; body : 'expr }
    | While_left of { invariant : 'expr; body : 'expr }
    | For of {
          index : mut_var
        ; init : 'expr
        ; invariant : 'expr
        ; variant : 'expr
        ; body : 'expr
      }
    | For_each of {indices : var list; collection : 'expr; body : 'expr}
    (* high-level control flow (iterative) *)
    | Map of { collection : 'expr; map : var list * 'expr }
    | Fold_left of { collection : 'expr; init : 'expr; fold : var * 'expr }
    | Fold_right of { collection : 'expr; init : 'expr; fold : var * 'expr }
    (* tuples *)
    | Let_tuple_in of { components : var list; rhs : 'expr; in_ : 'expr }
    | Tuple of 'expr row
    | Proj of 'expr * row_path
    | Update of { tuple : 'expr; component : row_path; update : 'expr }
    (* sums *)
    | Inj of row_path * 'expr
    | Match of 'expr * 'expr row
    (* tezos specific *)
    | Raw_michelson of micheline
    | Create_contract of 'expr contract
  [@@deriving map]
end

include Recursion.Fixpoint (Base)
