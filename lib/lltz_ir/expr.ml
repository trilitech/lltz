open Core
open Grace

type var = Var of string [@@ocaml.unboxed] [@@deriving sexp, equal, compare]
type mut_var = Mut_var of string [@@ocaml.unboxed] [@@deriving sexp, equal, compare]

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

type micheline = string Tezos_micheline.Micheline.canonical

type contract =
  { parameter : Type.t
  ; storage : Type.t
  ; code : t
  }

and row = t Rose_tree.t
and row_path = Rose_tree.Path.t

and t =
  { desc : desc
  ; range : Range.t
  }

and desc =
  (* basic lambda calculus w/ primitives + constants *)
  | Var of var
  | Let_in of
      { let_var : var
      ; rhs : t
      ; in_ : t
      }
  | Lambda of
      { lam_var : var
      ; return : Type.t
      ; body : t
      }
  | Lambda_rec of
      { lam_var : var
      ; mu_var : var
      ; return : Type.t
      ; body : t
      }
  | App of
      { abs : t
      ; arg : t
      }
  | Const of constant
  | Prim of Primitive.t * t list
  (* mutability *)
  | Let_mut_in of
      { let_var : mut_var
      ; rhs : t
      ; in_ : t
      }
  | Deref of mut_var
  | Assign of mut_var * t
  (* low-level control flow (conditional) *)
  | If_bool of
      { condition : t
      ; if_true : t
      ; if_false : t
      }
  | If_none of
      { subject : t
      ; if_none : t
      ; if_some : var * t
      }
  | If_cons of
      { subject : t
      ; if_empty : t
      ; if_nonempty : var * var * t
      }
  | If_left of
      { subject : t
      ; if_left : var * t
      ; if_right : var * t
      }
  (* low-level control flow (iterative) *)
  | While of
      { invariant : t
      ; body : t
      }
  | While_left of
      { invariant : t
      ; body : t
      }
  | For of
      { index : mut_var
      ; init : t
      ; invariant : t
      ; variant : t
      ; body : t
      }
  | For_each of
      { indices : var list
      ; collection : t
      ; body : t
      }
  (* high-level control flow (iterative) *)
  | Map of
      { collection : t
      ; map : var list * t
      }
  | Fold_left of
      { collection : t
      ; init : t
      ; fold : var * t
      }
  | Fold_right of
      { collection : t
      ; init : t
      ; fold : var * t
      }
  (* tuples *)
  | Let_tuple_in of
      { components : var list
      ; rhs : t
      ; in_ : t
      }
  | Tuple of row
  | Proj of t * row_path
  | Update of
      { tuple : t
      ; component : row_path
      ; update : t
      }
  (* sums *)
  | Inj of row_path * t
  | Match of t * row
  (* tezos specific *)
  | Raw_michelson of (micheline[@equal.ignore] [@compare.ignore] [@sexp.opaque])
  | Create_contract of contract
[@@deriving sexp, equal, compare]
