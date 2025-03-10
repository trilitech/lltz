open Core

module LLTZ = struct
  module E = Lltz_ir.Expr
  module T = Lltz_ir.Type
  module R = Lltz_ir.Row
  module P = Lltz_ir.Primitive
  module Dsl = Lltz_ir.Ast_builder.Default
  module Free_vars = Lltz_ir.Free_vars
  module Annotations = Lltz_ir.Annotations
end

let rec traverse_row
  (row : 'a LLTZ.R.t)
  (acc : String.Set.t)
  (f : 'a -> String.Set.t -> String.Set.t)
  : String.Set.t
  =
  match row with
  | LLTZ.R.Node rows ->
    List.fold_left ~f:(fun acc row -> traverse_row row acc f) ~init:acc rows
  | LLTZ.R.Leaf (_label_opt, a) -> f a acc
;;

(* We assume that all variable names are unique (ensured by frontend). *)

let collect_all_vars (expr : LLTZ.E.t) : String.Set.t =
  let rec aux (expr : LLTZ.E.t) acc =
    match expr.desc with
    | Variable (Var name) -> Set.add acc name
    | Let_in { let_var = Var var; rhs; in_ } ->
      let acc = aux rhs acc in
      let acc = aux in_ acc in
      Set.add acc var
    | Lambda { lam_var = Var var, _lam_var_type; body } ->
      let acc = aux body acc in
      Set.add acc var
    | Lambda_rec
        { mu_var = Var mu, _mu_type; lambda = { lam_var = Var var, _lam_var_type; body } }
      ->
      let acc = aux body acc in
      Set.add (Set.add acc var) mu
    | App { abs; arg } ->
      let acc = aux abs acc in
      aux arg acc
    | Const _constant -> acc
    | Prim (_primitive, args) ->
      List.fold_left ~f:(fun acc arg -> aux arg acc) ~init:acc args
    | Let_mut_in { let_var = Mut_var var; rhs; in_ } ->
      let acc = aux rhs acc in
      let acc = aux in_ acc in
      Set.add acc var
    | Deref (Mut_var var) -> Set.add acc var
    | Assign (Mut_var var, value) ->
      let acc = aux value acc in
      Set.add acc var
    | If_bool { condition; if_true; if_false } ->
      let acc = aux condition acc in
      let acc = aux if_true acc in
      aux if_false acc
    | If_none
        { subject; if_none; if_some = { lam_var = Var var, _var_type; body = some } } ->
      let acc = aux subject acc in
      let acc = aux if_none acc in
      let acc = aux some acc in
      Set.add acc var
    | If_cons
        { subject
        ; if_empty
        ; if_nonempty =
            { lam_var1 = Var hd, _var1_ty; lam_var2 = Var tl, _var2_ty; body = nonempty }
        } ->
      let acc = aux subject acc in
      let acc = aux if_empty acc in
      let acc = aux nonempty acc in
      Set.add (Set.add acc tl) hd
    | If_left
        { subject
        ; if_left = { lam_var = Var left, _left_ty; body = l }
        ; if_right = { lam_var = Var right, _right_ty; body = r }
        } ->
      let acc = aux subject acc in
      let acc = aux l acc in
      let acc = aux r acc in
      Set.add (Set.add acc right) left
    | While { cond; body } ->
      let acc = aux cond acc in
      aux body acc
    | While_left { cond; body = { lam_var = Var var, _var_ty; body = body_lambda } } ->
      let acc = aux cond acc in
      let acc = aux body_lambda acc in
      Set.add acc var
    | For { index = Mut_var var; init; cond; update; body } ->
      let acc = aux init acc in
      let acc = aux cond acc in
      let acc = aux update acc in
      let acc = aux body acc in
      Set.add acc var
    | For_each { collection; body = { lam_var = Var var, _var_ty; body = lambda_body } }
      ->
      let acc = aux collection acc in
      let acc = aux lambda_body acc in
      Set.add acc var
    | Map { collection; map = { lam_var = Var var, _var_ty; body = lam_body } } ->
      let acc = aux collection acc in
      let acc = aux lam_body acc in
      Set.add acc var
    | Fold_left
        { collection
        ; init = init_body
        ; fold = { lam_var = Var var, _var_ty; body = fold_body }
        } ->
      let acc = aux collection acc in
      let acc = aux init_body acc in
      let acc = aux fold_body acc in
      Set.add acc var
    | Fold_right
        { collection
        ; init = init_body
        ; fold = { lam_var = Var var, _var_ty; body = fold_body }
        } ->
      let acc = aux collection acc in
      let acc = aux init_body acc in
      let acc = aux fold_body acc in
      Set.add acc var
    | Let_tuple_in { components; rhs; in_ } ->
      let acc = aux rhs acc in
      let acc = aux in_ acc in
      List.fold_left ~f:(fun acc (Var var) -> Set.add acc var) ~init:acc components
    | Tuple row ->
      (* Traverse the Row.t structure for Tuple *)
      let acc = traverse_row row acc (fun expr acc -> aux expr acc) in
      acc
    | Proj (tuple, _path) -> aux tuple acc
    | Update { tuple; update; _ } ->
      let acc = aux tuple acc in
      aux update acc
    | Inj (_context, expr) -> aux expr acc
    | Match (subject, cases) ->
      let acc = aux subject acc in
      let acc =
        traverse_row cases acc (fun lambda acc ->
          let { lam_var = Var var, _; body } = lambda in
          let acc = aux body acc in
          Set.add acc var)
      in
      acc
    | Raw_michelson { args; _ } ->
      List.fold_left ~f:(fun acc arg -> aux arg acc) ~init:acc args
    | Create_contract
        { storage = _storage
        ; code = { lam_var = Var binder_var, _binder_ty; body = code_body }
        ; delegate
        ; initial_balance
        ; initial_storage
        } ->
      let acc = aux delegate acc in
      let acc = aux initial_balance acc in
      let acc = aux initial_storage acc in
      let acc = aux code_body acc in
      Set.add acc binder_var
    | Global_constant { hash = _hash; args } ->
      List.fold_left ~f:(fun acc arg -> aux arg acc) ~init:acc args
  in
  aux expr String.Set.empty
;;

(* We assume uniqueness of variable names here too (ensured by frontend).*)

let rec collect_used_vars (expr : LLTZ.E.t) : String.Set.t =
  match expr.desc with
  | Variable (Var v) -> String.Set.singleton v
  | Deref (Mut_var v) -> String.Set.singleton v
  | Let_in { let_var = Var _; rhs; in_ } ->
    Set.union (collect_used_vars rhs) (collect_used_vars in_)
  | Lambda { lam_var = Var _, _; body } -> collect_used_vars body
  | Lambda_rec { mu_var = Var _, _; lambda = { lam_var = Var _, _; body } } ->
    collect_used_vars body
  | App { abs; arg } -> Set.union (collect_used_vars abs) (collect_used_vars arg)
  | Const _ -> String.Set.empty
  | Prim (_, args) ->
    List.fold_left
      ~f:(fun acc a -> Set.union acc (collect_used_vars a))
      ~init:String.Set.empty
      args
  | Let_mut_in { let_var = Mut_var _; rhs; in_ } ->
    Set.union (collect_used_vars rhs) (collect_used_vars in_)
  | Assign (Mut_var _, value) -> collect_used_vars value
  | If_bool { condition; if_true; if_false } ->
    Set.union
      (collect_used_vars condition)
      (Set.union (collect_used_vars if_true) (collect_used_vars if_false))
  | If_none { subject; if_none; if_some } ->
    Set.union
      (collect_used_vars subject)
      (Set.union (collect_used_vars if_none) (collect_used_vars if_some.body))
  | If_cons { subject; if_empty; if_nonempty } ->
    Set.union
      (collect_used_vars subject)
      (Set.union (collect_used_vars if_empty) (collect_used_vars if_nonempty.body))
  | If_left { subject; if_left; if_right } ->
    Set.union
      (collect_used_vars subject)
      (Set.union (collect_used_vars if_left.body) (collect_used_vars if_right.body))
  | While { cond; body } -> Set.union (collect_used_vars cond) (collect_used_vars body)
  | While_left { cond; body } ->
    Set.union (collect_used_vars cond) (collect_used_vars body.body)
  | For { index = Mut_var _; init; cond; update; body } ->
    (* The loop index is not counted as used unless it appears as a Variable or Deref inside body etc. *)
    Set.union
      (collect_used_vars init)
      (Set.union
         (collect_used_vars cond)
         (Set.union (collect_used_vars update) (collect_used_vars body)))
  | For_each { collection; body } ->
    Set.union (collect_used_vars collection) (collect_used_vars body.body)
  | Map { collection; map } ->
    Set.union (collect_used_vars collection) (collect_used_vars map.body)
  | Fold_left { collection; init; fold } ->
    Set.union
      (collect_used_vars collection)
      (Set.union (collect_used_vars init) (collect_used_vars fold.body))
  | Fold_right { collection; init; fold } ->
    Set.union
      (collect_used_vars collection)
      (Set.union (collect_used_vars init) (collect_used_vars fold.body))
  | Let_tuple_in { components = _; rhs; in_ } ->
    Set.union (collect_used_vars rhs) (collect_used_vars in_)
  | Tuple row ->
    let rec collect_tuple (row : LLTZ.E.t LLTZ.R.t) =
      match row with
      | LLTZ.R.Leaf (_, e) -> collect_used_vars e
      | LLTZ.R.Node es ->
        List.fold_left
          ~f:(fun acc r -> Set.union acc (collect_tuple r))
          ~init:String.Set.empty
          es
    in
    collect_tuple row
  | Proj (tuple, _) -> collect_used_vars tuple
  | Update { tuple; component = _; update } ->
    Set.union (collect_used_vars tuple) (collect_used_vars update)
  | Inj (_, _) -> assert false
  | Match (_, _) -> assert false
  | Raw_michelson { michelson = _; args } ->
    List.fold_left
      ~f:(fun acc a -> Set.union acc (collect_used_vars a))
      ~init:String.Set.empty
      args
  | Create_contract { storage = _; code; delegate; initial_balance; initial_storage } ->
    Set.union
      (collect_used_vars delegate)
      (Set.union
         (collect_used_vars initial_balance)
         (Set.union (collect_used_vars initial_storage) (collect_used_vars code.body)))
  | Global_constant { hash = _; args } ->
    List.fold_left
      ~f:(fun acc a -> Set.union acc (collect_used_vars a))
      ~init:String.Set.empty
      args
;;

let compute_last_vars (expr : LLTZ.E.t) : LLTZ.E.t =
  let used_vars = collect_used_vars expr in
  let rec aux (expr : LLTZ.E.t) (live_set : String.Set.t) : LLTZ.E.t =
    let process_subexpr (expr : LLTZ.E.t) live_set =
      let expr' = aux expr live_set in
      let last_used_vars = expr'.annotations.last_used_vars in
      let live_set' = Set.diff live_set last_used_vars in
      expr', live_set'
    in
    let mk_annots ?(remove_never_used_vars = String.Set.empty) last_used_vars =
      LLTZ.Annotations.{ last_used_vars; remove_never_used_vars }
    in
    let process_args args live_set =
      let args_rev', _ =
        List.fold_left
          ~f:(fun (args_acc, live_set) arg ->
            let arg', live_set = process_subexpr arg live_set in
            arg' :: args_acc, live_set)
          ~init:([], live_set)
          args
      in
      List.rev args_rev'
    in
    let adjust_never_used_var (body : LLTZ.E.t) x =
      if not (Set.mem used_vars x)
      then
        { body with
          annotations =
            { body.annotations with
              remove_never_used_vars = Set.add body.annotations.remove_never_used_vars x
            }
        }
      else body
    in
    let process_lambda1 (lambda : LLTZ.E.lambda) live_set =
      let LLTZ.E.{ lam_var = Var x, ty; body } = lambda in
      let live_set = Set.add live_set x in
      let body', live_set = process_subexpr body live_set in
      let body' = adjust_never_used_var body' x in
      let live_set' = Set.remove live_set x in
      let lambda' = LLTZ.E.{ lam_var = Var x, ty; body = body' } in
      lambda', live_set'
    in
    let process_lambda2 (lambda2 : LLTZ.E.lambda2) live_set =
      let LLTZ.E.{ lam_var1 = Var x1, ty1; lam_var2 = Var x2, ty2; body } = lambda2 in
      let live_set = Set.add (Set.add live_set x1) x2 in
      let body', live_set = process_subexpr body live_set in
      let body' = adjust_never_used_var body' x1 in
      let body' = adjust_never_used_var body' x2 in
      let live_set' = Set.remove (Set.remove live_set x2) x1 in
      let lambda' =
        LLTZ.E.{ lam_var1 = Var x1, ty1; lam_var2 = Var x2, ty2; body = body' }
      in
      lambda', live_set'
    in
    let add_expr (e : LLTZ.E.t) acc = Set.union e.annotations.last_used_vars acc in
    let add_lambda1 (l : LLTZ.E.lambda) acc =
      match LLTZ.E.(l.lam_var) with
      | Var x, _ -> Set.add (add_expr l.body acc) x
    in
    let add_lambda2 (l : LLTZ.E.lambda2) acc =
      match LLTZ.E.(l.lam_var1), LLTZ.E.(l.lam_var2) with
      | (Var x1, _), (Var x2, _) -> Set.add (Set.add (add_expr l.body acc) x2) x1
    in
    let merge_branch_last_used (e1 : LLTZ.E.t) (e2 : LLTZ.E.t) =
      let to_add1 =
        Set.diff e2.annotations.last_used_vars e1.annotations.last_used_vars
      in
      let to_add2 =
        Set.diff e1.annotations.last_used_vars e2.annotations.last_used_vars
      in
      ( { e1 with
          annotations =
            { e1.annotations with
              remove_never_used_vars =
                Set.union e1.annotations.remove_never_used_vars to_add1
            }
        }
      , { e2 with
          annotations =
            { e2.annotations with
              remove_never_used_vars =
                Set.union e2.annotations.remove_never_used_vars to_add2
            }
        } )
    in
    match expr.desc with
    | Variable (Var v) ->
      let is_last_use = Set.mem live_set v in
      let last_used_vars =
        if is_last_use then String.Set.singleton v else String.Set.empty
      in
      { expr with annotations = mk_annots last_used_vars }
    | Let_in { let_var = Var x; rhs; in_ } ->
      (* TODO: possibility to further improve in the bodies of loops *)
      let live_set = Set.add live_set x in
      let in_', live_set = process_subexpr in_ live_set in
      let rhs', _ = process_subexpr rhs live_set in
      let last_used_vars = Set.add (add_expr rhs' (add_expr in_' String.Set.empty)) x in
      let remove_never_used_vars =
        if Set.mem used_vars x then String.Set.empty else String.Set.singleton x
      in
      { expr with
        desc = Let_in { let_var = Var x; rhs = rhs'; in_ = in_' }
      ; annotations = mk_annots ~remove_never_used_vars last_used_vars
      }
    | Lambda { lam_var = Var x, lam_var_type; body } ->
      let live_set = Set.add live_set x in
      let body', _ = process_subexpr body live_set in
      let last_used_vars = Set.add body'.annotations.last_used_vars x in
      { expr with
        desc = Lambda { lam_var = Var x, lam_var_type; body = body' }
      ; annotations = mk_annots last_used_vars
      }
    | Lambda_rec { mu_var = Var mu, mu_type; lambda } ->
      let lam_var_name =
        match lambda.lam_var with
        | Var x, _ -> x
      in
      let live_set = Set.add (Set.add live_set mu) lam_var_name in
      let body', _ = process_subexpr lambda.body live_set in
      let last_used_vars = Set.add (add_lambda1 lambda String.Set.empty) mu in
      let lambda' = { lambda with body = body' } in
      { expr with
        desc = Lambda_rec { mu_var = Var mu, mu_type; lambda = lambda' }
      ; annotations = mk_annots last_used_vars
      }
    | App { abs; arg } ->
      let abs', live_set = process_subexpr abs live_set in
      let arg', _ = process_subexpr arg live_set in
      let last_used_vars = add_expr abs' (add_expr arg' String.Set.empty) in
      { expr with
        desc = App { abs = abs'; arg = arg' }
      ; annotations = mk_annots last_used_vars
      }
    | Const _ -> { expr with annotations = mk_annots String.Set.empty }
    | Prim (primitive, args) ->
      (* Process arguments from last to first *)
      let args' = process_args args live_set in
      let last_used_vars =
        List.fold_left ~f:(fun acc arg -> add_expr arg acc) ~init:String.Set.empty args'
      in
      { expr with desc = Prim (primitive, args'); annotations = mk_annots last_used_vars }
    | Let_mut_in { let_var = Mut_var x; rhs; in_ } ->
      let live_set = Set.add live_set x in
      let in_', live_set = process_subexpr in_ live_set in
      let rhs', _ = process_subexpr rhs live_set in
      let last_used_vars = Set.add (add_expr rhs' (add_expr in_' String.Set.empty)) x in
      let remove_never_used_vars =
        if Set.mem used_vars x then String.Set.empty else String.Set.singleton x
      in
      { expr with
        desc = Let_mut_in { let_var = Mut_var x; rhs = rhs'; in_ = in_' }
      ; annotations = mk_annots ~remove_never_used_vars last_used_vars
      }
    | Deref (Mut_var v) ->
      let is_last_use = Set.mem live_set v in
      let last_used_vars =
        if is_last_use then String.Set.singleton v else String.Set.empty
      in
      { expr with annotations = mk_annots last_used_vars }
    | Assign (Mut_var x, value) ->
      let is_last_use = Set.mem live_set x in
      let value', _ = process_subexpr value live_set in
      let last_used_vars = value'.annotations.last_used_vars in
      let last_used_vars =
        if is_last_use then Set.add last_used_vars x else last_used_vars
      in
      let remove_never_used_vars =
        if Set.mem used_vars x then String.Set.empty else String.Set.singleton x
      in
      { expr with
        desc = Assign (Mut_var x, value')
      ; annotations = mk_annots ~remove_never_used_vars last_used_vars
      }
    | If_bool { condition; if_true; if_false } ->
      let if_false', live_set_left = process_subexpr if_false live_set in
      let if_true', live_set_right = process_subexpr if_true live_set in
      let live_set = Set.inter live_set_left live_set_right in
      let condition', _ = process_subexpr condition live_set in
      let last_used_vars =
        add_expr condition' (add_expr if_true' (add_expr if_false' String.Set.empty))
      in
      let if_false', if_true' = merge_branch_last_used if_false' if_true' in
      { expr with
        desc =
          If_bool { condition = condition'; if_true = if_true'; if_false = if_false' }
      ; annotations = mk_annots last_used_vars
      }
    | If_none { subject; if_none; if_some } ->
      let if_none', live_set_none = process_subexpr if_none live_set in
      let if_some', live_set_some = process_lambda1 if_some live_set in
      let live_set = Set.inter live_set_none live_set_some in
      let subject', _ = process_subexpr subject live_set in
      let last_used_vars =
        add_expr subject' (add_expr if_none' (add_lambda1 if_some' String.Set.empty))
      in
      let if_none', if_some_body = merge_branch_last_used if_none' if_some'.body in
      { expr with
        desc =
          If_none
            { subject = subject'
            ; if_none = if_none'
            ; if_some = { if_some' with body = if_some_body }
            }
      ; annotations = mk_annots last_used_vars
      }
    | If_cons { subject; if_empty; if_nonempty } ->
      let if_empty', live_set_empty = process_subexpr if_empty live_set in
      let if_nonempty', live_set_nonempty = process_lambda2 if_nonempty live_set in
      let live_set = Set.inter live_set_empty live_set_nonempty in
      let subject', _ = process_subexpr subject live_set in
      let last_used_vars =
        add_expr subject' (add_expr if_empty' (add_lambda2 if_nonempty' String.Set.empty))
      in
      let if_empty', if_nonempty_body =
        merge_branch_last_used if_empty' if_nonempty'.body
      in
      { expr with
        desc =
          If_cons
            { subject = subject'
            ; if_empty = if_empty'
            ; if_nonempty = { if_nonempty' with body = if_nonempty_body }
            }
      ; annotations = mk_annots last_used_vars
      }
    | If_left { subject; if_left; if_right } ->
      let if_right', live_set_right = process_lambda1 if_right live_set in
      let if_left', live_set_left = process_lambda1 if_left live_set in
      let live_set = Set.inter live_set_left live_set_right in
      let subject', _ = process_subexpr subject live_set in
      let last_used_vars =
        add_expr subject' (add_lambda1 if_left' (add_lambda1 if_right' String.Set.empty))
      in
      let if_left_body, if_right_body =
        merge_branch_last_used if_left'.body if_right'.body
      in
      { expr with
        desc =
          If_left
            { subject = subject'
            ; if_left = { if_left' with body = if_left_body }
            ; if_right = { if_right' with body = if_right_body }
            }
      ; annotations = mk_annots last_used_vars
      }
    | While { cond; body } ->
      let body'', live_set = process_subexpr body live_set in
      let cond'', _ = process_subexpr cond live_set in
      let last_used_vars = add_expr cond'' (add_expr body'' String.Set.empty) in
      (* Optimizing internal variables only as other can be reused in next cycle*)
      let body', _ = process_subexpr body String.Set.empty in
      let cond', _ = process_subexpr cond String.Set.empty in
      { expr with
        desc = While { cond = cond'; body = body' }
      ; annotations = mk_annots last_used_vars
      }
    | While_left { cond; body } ->
      let body'', live_set = process_lambda1 body live_set in
      let cond'', _ = process_subexpr cond live_set in
      let last_used_vars = add_expr cond'' (add_lambda1 body'' String.Set.empty) in
      (* Optimizing internal variables only as other can be reused in next cycle. *)
      let body', _ = process_lambda1 body String.Set.empty in
      { expr with
        desc = While_left { cond = cond''; body = body' }
      ; annotations = mk_annots last_used_vars
      }
    | For { index = Mut_var x; init; cond; update; body } ->
      let live_set = Set.remove live_set x in
      let body'', live_set = process_subexpr body live_set in
      let update'', live_set = process_subexpr update live_set in
      let cond'', live_set = process_subexpr cond live_set in
      let init'', _ = process_subexpr init live_set in
      let last_used_vars =
        Set.add
          (add_expr
             init''
             (add_expr cond'' (add_expr update'' (add_expr body'' String.Set.empty))))
          x
      in
      (* Optimizing internal variables only as other can be reused in next cycle. *)
      let body', _ = process_subexpr body String.Set.empty in
      let update', _ = process_subexpr update String.Set.empty in
      let cond', _ = process_subexpr cond String.Set.empty in
      { expr with
        desc =
          For
            { index = Mut_var x
            ; init = init''
            ; cond = cond'
            ; update = update'
            ; body = body'
            }
      ; annotations = mk_annots last_used_vars
      }
    | For_each { collection; body } ->
      let body', live_set = process_lambda1 body live_set in
      let collection', _ = process_subexpr collection live_set in
      let last_used_vars = add_expr collection' (add_lambda1 body' String.Set.empty) in
      let body'', _ = process_lambda1 body String.Set.empty in
      { expr with
        desc = For_each { collection = collection'; body = body'' }
      ; annotations = mk_annots last_used_vars
      }
    | Map { collection; map } ->
      let map', live_set = process_lambda1 map live_set in
      let collection', _ = process_subexpr collection live_set in
      let last_used_vars = add_expr collection' (add_lambda1 map' String.Set.empty) in
      let map'', _ = process_lambda1 map String.Set.empty in
      { expr with
        desc = Map { collection = collection'; map = map'' }
      ; annotations = mk_annots last_used_vars
      }
    | Fold_left { collection; init; fold } ->
      let fold', live_set = process_lambda1 fold live_set in
      let collection', live_set = process_subexpr collection live_set in
      let init', _ = process_subexpr init live_set in
      let last_used_vars =
        add_expr collection' (add_expr init' (add_lambda1 fold' String.Set.empty))
      in
      let fold'', _ = process_lambda1 fold String.Set.empty in
      { expr with
        desc = Fold_left { collection = collection'; init = init'; fold = fold'' }
      ; annotations = mk_annots last_used_vars
      }
    | Fold_right { collection; init; fold } ->
      let fold', live_set = process_lambda1 fold live_set in
      let collection', live_set = process_subexpr collection live_set in
      let init', _ = process_subexpr init live_set in
      let last_used_vars =
        add_expr collection' (add_expr init' (add_lambda1 fold' String.Set.empty))
      in
      let fold'', _ = process_lambda1 fold String.Set.empty in
      { expr with
        desc = Fold_right { collection = collection'; init = init'; fold = fold'' }
      ; annotations = mk_annots last_used_vars
      }
    | Let_tuple_in { components; rhs; in_ } ->
      let live_set =
        List.fold_left ~f:(fun acc (Var x) -> Set.add acc x) ~init:live_set components
      in
      let in_', live_set = process_subexpr in_ live_set in
      let rhs', _ = process_subexpr rhs live_set in
      let last_used_vars =
        add_expr
          rhs'
          (add_expr
             in_'
             (List.fold_left
                ~f:(fun acc (Var x) -> Set.add acc x)
                ~init:String.Set.empty
                components))
      in
      let remove_never_used_vars =
        List.fold_left
          ~f:(fun acc (Var x) -> if Set.mem used_vars x then acc else Set.add acc x)
          ~init:String.Set.empty
          components
      in
      { expr with
        desc = Let_tuple_in { components; rhs = rhs'; in_ = in_' }
      ; annotations = mk_annots ~remove_never_used_vars last_used_vars
      }
    | Tuple row ->
      let rec aux_tuple (row : LLTZ.E.t LLTZ.R.t) (live_set : String.Set.t)
        : LLTZ.E.t LLTZ.R.t * String.Set.t
        =
        match row with
        | LLTZ.R.Node rows ->
          let rows', live_set =
            List.fold_left
              ~f:(fun (rows_acc, live_set) row ->
                let row', live_set = aux_tuple row live_set in
                row' :: rows_acc, live_set)
              ~init:([], live_set)
              rows
          in
          LLTZ.R.Node (List.rev rows'), live_set
        | LLTZ.R.Leaf (label_opt, expr) ->
          let expr', live_set = process_subexpr expr live_set in
          LLTZ.R.Leaf (label_opt, expr'), live_set
      in
      let row', new_live_set = aux_tuple row live_set in
      let last_used_vars = Set.diff live_set new_live_set in
      { expr with desc = Tuple row'; annotations = mk_annots last_used_vars }
    | Proj (tuple, path) ->
      let tuple', _ = process_subexpr tuple live_set in
      let last_used_vars = tuple'.annotations.last_used_vars in
      { expr with desc = Proj (tuple', path); annotations = mk_annots last_used_vars }
    | Update { tuple; component; update } ->
      let update', live_set = process_subexpr update live_set in
      let tuple', _ = process_subexpr tuple live_set in
      let last_used_vars = add_expr tuple' (add_expr update' String.Set.empty) in
      { expr with
        desc = Update { tuple = tuple'; component; update = update' }
      ; annotations = mk_annots last_used_vars
      }
    | Inj (_, _) ->
      assert false
      (*
         TODO: Will be used once connected to SmartPy
      { expr with annotations = mk_annots live_set }*)
      (*let (expr_inj', live_set) = process_subexpr expr_inj live_set in
      let last_used_vars = expr_inj'.annotations.last_used_vars in
      { expr with desc = Inj (context, expr_inj'); annotations = mk_annots last_used_vars }*)
    | Match (_, _) ->
      assert false
      (*
         TODO: Will be used once connected to SmartPy
      { expr with annotations = mk_annots live_set }*)
      (*let (cases', live_set, last_used_vars_cases) = process_row_cases cases live_set in
      let (subject', live_set) = process_subexpr subject live_set in
      let last_used_vars = Set.union subject'.annotations.last_used_vars last_used_vars_cases in
      { expr with desc = Match (subject', cases'); annotations = mk_annots last_used_vars }*)
    | Raw_michelson { michelson; args } ->
      let args' = process_args args live_set in
      let last_used_vars =
        List.fold_left ~f:(fun acc arg -> add_expr arg acc) ~init:String.Set.empty args'
      in
      { expr with
        desc = Raw_michelson { michelson; args = args' }
      ; annotations = mk_annots last_used_vars
      }
    | Create_contract { storage; code; delegate; initial_balance; initial_storage } ->
      let code', live_set = process_lambda1 code live_set in
      let delegate', live_set = process_subexpr delegate live_set in
      let initial_balance', live_set = process_subexpr initial_balance live_set in
      let initial_storage', _ = process_subexpr initial_storage live_set in
      let last_used_vars =
        add_expr
          delegate'
          (add_expr
             initial_balance'
             (add_expr initial_storage' (add_lambda1 code' String.Set.empty)))
      in
      { expr with
        desc =
          Create_contract
            { storage
            ; code = code'
            ; delegate = delegate'
            ; initial_balance = initial_balance'
            ; initial_storage = initial_storage'
            }
      ; annotations = mk_annots last_used_vars
      }
    | Global_constant { hash; args } ->
      let args' = process_args args live_set in
      let last_used_vars =
        List.fold_left ~f:(fun acc arg -> add_expr arg acc) ~init:String.Set.empty args'
      in
      { expr with
        desc = Global_constant { hash; args = args' }
      ; annotations = mk_annots last_used_vars
      }
  in
  let init_live_set = collect_all_vars expr in
  aux expr init_live_set
;;
