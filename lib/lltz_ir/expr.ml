open Import
open Grace

type micheline = (Range.t, string) Tezos_micheline.Micheline.node

module T = struct
  type var = Var of string 
  and mut_var = Mut_var of string

  and constant = 
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

  and t =
    { desc : desc
    ; range : Range.t
    }

  and desc =
    (* basic lambda calculus w/ primitives + constants *)
    | Variable of var
    | Let_in of
        { let_var : var
        ; rhs : t
        ; in_ : t
        }
    | Lambda of
        { lam_var : var * Type.t
        ; return_type : Type.t
        ; body : t
        }
    | Lambda_rec of
        { lam_var : var * Type.t 
        ; mu_var : var
        ; return_type : Type.t
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
        ; init : var * t
        ; fold : var * t
        }
    | Fold_right of
        { collection : t
        ; init : var * t
        ; fold : var * t
        }
    (* tuples *)
    | Let_tuple_in of
        { components : var list
        ; rhs : t
        ; in_ : t
        }
    | Tuple of t Row.t
    | Proj of t * Row.Path.t
    | Update of
        { tuple : t
        ; component : Row.Path.t
        ; update : t
        }
    (* sums *)
    | Inj of Type.t Row.Context.t * t
    | Match of t * (var * t) Row.t
    (* tezos specific *)
    | Raw_michelson of (micheline[@sexp.opaque] [@equal.ignore] [@compare.ignore])
    | Create_contract of
        { storage : Type.t
        ; parameter : Type.t
        ; code : t
        ; delegate : t
        ; initial_balance : t
        ; initial_storage : t
        }
  [@@deriving sexp, equal, compare, traverse]
end

include T

(*
module Traverse = struct
  class map =
    let zero = Traverse_builtins.map_zero in
    object
      inherit Traverse_builtins.map
      inherit Row.Traverse_builtins.map
      inherit T.map
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end

  class iter =
    let zero = Traverse_builtins.iter_zero in
    object
      inherit Traverse_builtins.iter
      inherit Row.Traverse_builtins.iter
      inherit T.iter
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end

  class ['acc] fold =
    let zero = Traverse_builtins.fold_zero in
    object
      inherit ['acc] Traverse_builtins.fold
      inherit ['acc] Row.Traverse_builtins.fold
      inherit ['acc] T.fold
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end

  class ['acc] fold_map =
    let zero = Traverse_builtins.fold_map_zero in
    object
      inherit ['acc] Traverse_builtins.fold_map
      inherit ['acc] Row.Traverse_builtins.fold_map
      inherit ['acc] T.fold_map
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end

  class ['ctx] map_with_context =
    let zero = Traverse_builtins.map_with_context_zero in
    object
      inherit ['ctx] Traverse_builtins.map_with_context
      inherit ['ctx] Row.Traverse_builtins.map_with_context
      inherit ['ctx] T.map_with_context
      method z__t = zero
      method type__t = zero
      method row__path__t = zero
      method range__t = zero
      method primitive__t = zero
      method micheline = zero
    end
end

*)