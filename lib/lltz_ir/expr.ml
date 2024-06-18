open Core
open Common

type type_ = location Type.t [@@deriving sexp, equal, compare]
type 'a row = 'a Type.row [@@deriving sexp, equal, compare]
type row_path = Type.row_path [@@deriving sexp, equal, compare]
type var = Var of string [@@deriving sexp, equal, compare]
type mut_var = Mut_var of string [@@deriving sexp, equal, compare]

type constant =
  | Unit
  | Bool of bool
  | Nat of (Z.t[@sexp.opaque])
  | Int of (Z.t[@sexp.opaque])
  | Mutez of (Z.t[@sexp.opaque])
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
[@@deriving sexp, equal, compare]

type micheline = (location, string) Tezos_micheline.Micheline.node

module T = struct
  type 'expr t =
    (* basic lambda calculus w/ primitives + constants *)
    | Var of var
    | Let_in of { let_var : var; rhs : 'expr; in_ : 'expr }
    | Lambda of { lam_var : var; return : type_; body : 'expr }
    | Lambda_rec of {
          lam_var : var
        ; mu_var : var
        ; return : type_
        ; body : 'expr
      }
    | App of { abs : 'expr; arg : 'expr }
    | Const of constant
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
    | For_each of { indices : var list; collection : 'expr; body : 'expr }
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
    | Raw_michelson of
        (micheline[@sexp.opaque] [@equal.ignore] [@compare.ignore])
    | Create_contract of { storage : type_; parameter : type_; code : 'expr }
  [@@deriving sexp, equal, compare]

  let map t ~f =
    match t with
    | Var v -> Var v
    | Let_in { let_var; rhs; in_ } ->
        Let_in { let_var; rhs = f rhs; in_ = f in_ }
    | Lambda { lam_var; return; body } ->
        Lambda { lam_var; return; body = f body }
    | Lambda_rec { lam_var; mu_var; return; body } ->
        Lambda_rec { lam_var; mu_var; return; body = f body }
    | App { abs; arg } -> App { abs = f abs; arg = f arg }
    | Const c -> Const c
    | Prim0 p -> Prim0 p
    | Prim1 (p, e) -> Prim1 (p, f e)
    | Prim2 (p, e1, e2) -> Prim2 (p, f e1, f e2)
    | Prim3 (p, e1, e2, e3) -> Prim3 (p, f e1, f e2, f e3)
    | Let_mut_in { let_var; rhs; in_ } ->
        Let_mut_in { let_var; rhs = f rhs; in_ = f in_ }
    | Deref v -> Deref v
    | Assign (v, e) -> Assign (v, f e)
    | If_bool { condition; if_true; if_false } ->
        If_bool
          {
            condition = f condition
          ; if_true = f if_true
          ; if_false = f if_false
          }
    | If_none { subject; if_none; if_some = x, if_some } ->
        If_none
          { subject = f subject; if_none = f if_none; if_some = (x, f if_some) }
    | If_cons { subject; if_empty; if_nonempty = x, xs, if_nonempty } ->
        If_cons
          {
            subject = f subject
          ; if_empty = f if_empty
          ; if_nonempty = (x, xs, f if_nonempty)
          }
    | If_left { subject; if_left = x, if_left; if_right = y, if_right } ->
        If_left
          {
            subject = f subject
          ; if_left = (x, f if_left)
          ; if_right = (y, f if_right)
          }
    | While { invariant; body } ->
        While { invariant = f invariant; body = f body }
    | While_left { invariant; body } ->
        While_left { invariant = f invariant; body = f body }
    | For { index; init; invariant; variant; body } ->
        For
          {
            index
          ; init = f init
          ; invariant = f invariant
          ; variant = f variant
          ; body = f body
          }
    | For_each { indices; collection; body } ->
        For_each { indices; collection = f collection; body = f body }
    | Map { collection; map = xs, map } ->
        Map { collection = f collection; map = (xs, f map) }
    | Fold_left { collection; init; fold = xs, fold } ->
        Fold_left
          { collection = f collection; init = f init; fold = (xs, f fold) }
    | Fold_right { collection; init; fold = xs, fold } ->
        Fold_right
          { collection = f collection; init = f init; fold = (xs, f fold) }
    | Let_tuple_in { components; rhs; in_ } ->
        Let_tuple_in { components; rhs = f rhs; in_ = f in_ }
    | Tuple row -> Tuple (Rose_tree.map row ~f)
    | Proj (e, p) -> Proj (f e, p)
    | Update { tuple; component; update } ->
        Update { tuple = f tuple; component; update = f update }
    | Inj (p, e) -> Inj (p, f e)
    | Match (e, row) -> Match (f e, Rose_tree.map row ~f)
    | Raw_michelson m -> Raw_michelson m
    | Create_contract { storage; parameter; code } ->
        Create_contract { storage; parameter; code = f code }
end

include Recursion.Fixpoint (T)
