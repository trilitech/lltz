open Core

module LLTZ = struct
  module E = Expr
  module T = Type
  module P = Primitive
  module R = Row
end

(* Computes a map from free variables to their types in the given expression *)
let free_vars_with_types (expr : LLTZ.E.t) : LLTZ.T.t String.Map.t =
  let empty = String.Map.empty in
  (* Merge two maps, ensuring consistent types for identical variables *)
  let merge fvs1 fvs2 =
    Map.merge_skewed fvs1 fvs2 ~combine:(fun ~key:ident type1 type2 ->
      if LLTZ.T.equal_types type1 type2
      then type1
      else
        raise_s
          [%message
            "free_vars_with_types: inconsistent types in free variables"
              (ident : string)
              (type1 : LLTZ.T.t)
              (type2 : LLTZ.T.t)])
  in
  (* Merge a list of maps *)
  let merge_all fvss = List.fold fvss ~init:empty ~f:merge in
  (* Remove bound variables from the map of free variables *)
  let remove fvs idents = List.fold idents ~init:fvs ~f:Map.remove in
  (* Recursively find free variables with types in the expression *)
  let rec loop (expr : LLTZ.E.t) (bound_vars : string list) : LLTZ.T.t String.Map.t =
    match expr.desc with
    | Variable (Var name) ->
      if List.mem bound_vars name ~equal:String.equal
      then empty
      else String.Map.singleton name expr.type_
    | Let_in { let_var = Var var; rhs; in_ } ->
      merge (remove (loop in_ (var :: bound_vars)) [ var ]) (loop rhs bound_vars)
    | Lambda { lam_var = Var var, var_type; body } -> loop body (var :: bound_vars)
    | Lambda_rec
        { mu_var = Var mu, mu_type; lambda = { lam_var = Var var, lam_var_type; body } }
      -> loop body (mu :: var :: bound_vars)
    | App { abs; arg } -> merge (loop abs bound_vars) (loop arg bound_vars)
    | Const _ -> empty
    | Prim (_, args) -> merge_all (List.map args ~f:(fun arg -> loop arg bound_vars))
    | Let_mut_in { let_var = Mut_var var; rhs; in_ } ->
      merge (remove (loop in_ (var :: bound_vars)) [ var ]) (loop rhs bound_vars)
    | Deref (Mut_var var) ->
      if List.mem bound_vars var ~equal:String.equal
      then empty
      else String.Map.singleton var expr.type_
    | Assign (Mut_var var, value) -> loop value bound_vars
    | If_bool { condition; if_true; if_false } ->
      merge_all
        [ loop condition bound_vars; loop if_true bound_vars; loop if_false bound_vars ]
    | If_none { subject; if_none; if_some = { lam_var = Var var, var_type; body } } ->
      let subject_fvs = loop subject bound_vars in
      let if_none_fvs = loop if_none bound_vars in
      let some_fvs = loop body (var :: bound_vars) in
      merge_all [ subject_fvs; if_none_fvs; some_fvs ]
    | If_cons
        { subject
        ; if_empty
        ; if_nonempty = { lam_var1 = Var hd, var1_ty; lam_var2 = Var tl, var2_ty; body }
        } ->
      let subject_fvs = loop subject bound_vars in
      let if_empty_fvs = loop if_empty bound_vars in
      let nonempty_fvs = loop body (hd :: tl :: bound_vars) in
      merge_all [ subject_fvs; if_empty_fvs; nonempty_fvs ]
    | If_left
        { subject
        ; if_left = { lam_var = Var left, left_ty; body = l }
        ; if_right = { lam_var = Var right, right_ty; body = r }
        } ->
      let subject_fvs = loop subject bound_vars in
      let left_fvs = loop l (left :: bound_vars) in
      let right_fvs = loop r (right :: bound_vars) in
      merge_all [ subject_fvs; left_fvs; right_fvs ]
    | While { cond; body } -> merge (loop cond bound_vars) (loop body bound_vars)
    | While_left { cond; body = { lam_var = Var var, var_ty; body = body_lambda } } ->
      let cond_fvs = loop cond bound_vars in
      let body_fvs = loop body_lambda (var :: bound_vars) in
      merge cond_fvs body_fvs
    | For { index = Mut_var var; init; cond; update; body } ->
      let init_fvs = loop init bound_vars in
      let cond_fvs = loop cond (var :: bound_vars) in
      let update_fvs = loop update (var :: bound_vars) in
      let body_fvs = loop body (var :: bound_vars) in
      merge_all [ init_fvs; cond_fvs; update_fvs; body_fvs ]
    | For_each { collection; body = { lam_var = Var var, var_ty; body } } ->
      merge (loop collection bound_vars) (remove (loop body (var :: bound_vars)) [ var ])
    | Map { collection; map = { lam_var = Var var, var_ty; body = lam_body } } ->
      let collection_fvs = loop collection bound_vars in
      let body_fvs = loop lam_body (var :: bound_vars) in
      merge collection_fvs body_fvs
    | Fold_left
        { collection
        ; init = init_body
        ; fold = { lam_var = Var var, var_ty; body = fold_body }
        } ->
      let collection_fvs = loop collection bound_vars in
      let init_fvs = loop init_body bound_vars in
      let fold_fvs = loop fold_body (var :: bound_vars) in
      merge_all [ collection_fvs; init_fvs; fold_fvs ]
    | Fold_right
        { collection
        ; init = init_body
        ; fold = { lam_var = Var var, var_ty; body = fold_body }
        } ->
      let collection_fvs = loop collection bound_vars in
      let init_fvs = loop init_body bound_vars in
      let fold_fvs = loop fold_body (var :: bound_vars) in
      merge_all [ collection_fvs; init_fvs; fold_fvs ]
    | Let_tuple_in { components; rhs; in_ } ->
      let component_names = List.map components ~f:(fun (Var var) -> var) in
      merge
        (remove (loop in_ (component_names @ bound_vars)) component_names)
        (loop rhs bound_vars)
    | Tuple row -> compile_row_free_vars_with_types row bound_vars
    | Proj (tuple, _) -> loop tuple bound_vars
    | Update { tuple; component; update } ->
      merge (loop tuple bound_vars) (loop update bound_vars)
    | Inj (_, expr) -> loop expr bound_vars
    | Match (subject, cases) ->
      let subject_fvs = loop subject bound_vars in
      let cases_fvs = compile_row_cases_free_vars_with_types cases bound_vars in
      merge subject_fvs cases_fvs
    | Raw_michelson { michelson; args } ->
      merge_all (List.map args ~f:(fun arg -> loop arg bound_vars))
    | Create_contract
        { storage
        ; code = { lam_var = Var param_var, param_ty; body = code_body }
        ; delegate
        ; initial_balance
        ; initial_storage
        } ->
      let code_body_fvs = loop code_body (param_var :: bound_vars) in
      let delegate_fvs = loop delegate bound_vars in
      let initial_balance_fvs = loop initial_balance bound_vars in
      let initial_storage_fvs = loop initial_storage bound_vars in
      merge_all [ code_body_fvs; delegate_fvs; initial_balance_fvs; initial_storage_fvs ]
    | Global_constant { hash; args } ->
      merge_all (List.map args ~f:(fun arg -> loop arg bound_vars))
  and compile_row_free_vars_with_types row bound_vars =
    match row with
    | LLTZ.R.Node nodes ->
      List.map nodes ~f:(fun node -> compile_row_free_vars_with_types node bound_vars)
      |> merge_all
    | LLTZ.R.Leaf (_, expr) -> loop expr bound_vars
  and compile_row_cases_free_vars_with_types cases bound_vars =
    match cases with
    | LLTZ.R.Node nodes ->
      List.map nodes ~f:(fun case ->
        compile_row_cases_free_vars_with_types case bound_vars)
      |> merge_all
    | LLTZ.R.Leaf (_, { lam_var = Var var, var_ty; body }) ->
      remove (loop body (var :: bound_vars)) [ var ]
  in
  loop expr []
;;
