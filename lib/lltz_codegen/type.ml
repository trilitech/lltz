module M = Lltz_michelson.Ast
module T = M.Type

let tuple ?(annots = []) types =
  (* Right-comb encoding of tuple-types *)
  let rec loop ?(annots = []) types =
    match types, annots with
    | [], [] -> assert false
    | [ type1 ], _ -> type1
    | [], _ :: _ -> assert false
    | [ type1; type2 ], [ annot1; annot2 ] -> T.pair ~annot1 ~annot2 type1 type2
    | [ type1; type2 ], [ annot1 ] -> T.pair ~annot1 type1 type2
    | [ type1; type2 ], _ -> T.pair type1 type2
    | type_ :: types, annot1 :: tl_annots ->
      T.pair ~annot1 type_ (loop types ~annots:tl_annots)
    | type_ :: types, _ -> T.pair type_ (loop types)
  in
  loop types ~annots
;;

let ors ?(annots = []) types =
  (* Right-comb encoding of or-types (not efficient, but cheap) *)
  let rec loop ?(annots = []) types =
    match types, annots with
    | [], [] | [ _ ], [ _ ] | [], _ :: _ -> assert false
    | [ type1; type2 ], [ annot1; annot2 ] -> T.or_ ~annot1 ~annot2 type1 type2
    | [ type1; type2 ], [ annot1 ] -> T.or_ ~annot1 type1 type2
    | [ type1; type2 ], _ -> T.or_ type1 type2
    | type_ :: types, annot1 :: tl_annots ->
      T.or_ ~annot1 type_ (loop types ~annots:tl_annots)
    | type_ :: types, _ -> T.or_ type_ (loop types)
  in
  loop types ~annots
;;
