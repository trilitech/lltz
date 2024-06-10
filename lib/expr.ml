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

  type 'expr contract = { parameter : type_; storage : type_; body : 'expr }
  [@@deriving map]

  type micheline = (location, string) Tezos_micheline.Micheline.node list

  let map_row = Rose_tree.map

  type 'expr t =
    (* basic lambda calculus w/ primivatives + constants *)
    | Var of var
    | Let_in of { binding : var; let_ : 'expr; in_ : 'expr }
    | Lambda of { binding : var; ty : type_; body : 'expr }
    | Lambda_rec of { binding : var; recurse : var; ty : type_; body : 'expr }
    | App of { func : 'expr; arg : 'expr }
    | Const of const
    | Prim0 of type_ Primitive.t0
    | Prim1 of type_ Primitive.t1 * 'expr
    | Prim2 of type_ Primitive.t2 * 'expr * 'expr
    | Prim3 of type_ Primitive.t3 * 'expr * 'expr * 'expr
    (* mutability *)
    | Let_mut_in of { binding : mut_var; let_ : 'expr; in_ : 'expr }
    | Deref of mut_var
    | Assign of mut_var * 'expr
    (* low-level control flow (conditional) *)
    | If_bool of { condition : 'expr; on_true : 'expr; on_false : 'expr }
    | If_none of { scrutinee : 'expr; on_none : 'expr; on_some : var * 'expr }
    | If_cons of {
          scrutinee : 'expr
        ; on_empty : 'expr
        ; on_nonempty : var * var * 'expr
      }
    | If_left of {
          scrutinee : 'expr
        ; on_left : var * 'expr
        ; on_right : var * 'expr
      }
    (* low-level control flow (iterative) *)
    | While of { condition : 'expr; body : 'expr }
    | While_left of { condition : 'expr; body : 'expr }
    | For of {
          counter : mut_var
        ; init : 'expr
        ; condition : 'expr
        ; update : 'expr
        ; body : 'expr
      }
    | For_each of var list * 'expr * 'expr
    (* high-level control flow (iterative) *)
    | Map of { container : 'expr; function_ : var list * 'expr }
    | Fold_left of { container : 'expr; init : 'expr; fold : var * 'expr }
    | Fold_right of { container : 'expr; init : 'expr; fold : var * 'expr }
    (* tuples *)
    | Let_tuple_in of { bindings : var list; let_ : 'expr; in_ : 'expr }
    | Tuple of 'expr row
    | Proj of 'expr * row_path
    | Update of { tuple : 'expr; element : row_path; update : 'expr }
    (* sums *)
    | Inj of row_path * 'expr
    | Match of 'expr * 'expr row
    (* tezos specific *)
    | Raw_michelson of micheline
    | Create_contract of 'expr contract
  [@@deriving map]
end

include Recursion.Fixpoint (Base)
