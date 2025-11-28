module M = Lltz_michelson.Ast
module T = M.Type

let tuple ?(annot = None) types =
  (* Right-comb encoding of tuple-types *)
  let rec loop ?(annot = None) types =
    match types with
    | [] -> assert false (* empy tuples not supported, ensured by frontend*)
    | [ type1 ] -> type1
    | [ type1; type2 ] -> T.pair ~annot type1 type2
    | type_ :: types -> T.pair ~annot type_ (loop types)
  in
  loop types ~annot
;;

let ors ?(annot = None) types =
  (* Right-comb encoding of or-types (not efficient, but cheap) *)
  let rec loop ?(annot = None) types =
    match types with
    | [] | [ _ ] ->
      assert false (* ors of less than 2 types not supported, ensured by frontend*)
    | [ type1; type2 ] -> T.or_ ~annot type1 type2
    | type_ :: types -> T.or_ ~annot type_ (loop types)
  in
  loop types ~annot
;;
