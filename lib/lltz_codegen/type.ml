module M = Michelson.Ast 
module T = M.Type

let tuple ?(annot = None) types =
  (* Right-comb encoding of tuple-types *)
  let rec loop ?(annot = None) types =
    match types with
    | [] -> assert false
    | [ type1 ] -> type1
    | [ type1; type2 ] -> T.pair ~annot type1 type2
    | type_ :: types -> T.pair ~annot type_ (loop types)
  in
  loop types ~annot
;;

(*let rec tuple_left types =
  (* Left-comb encoding of tuple-types *)
  match types with
  | [] -> T.unit
  | [ type_ ] -> type_
  | a::b::types -> tuple_left ((T.pair [a;b])::types)*)

let ors ?(annot = None) types =
  (* Right-comb encoding of or-types (not efficient, but cheap) *)
  let rec loop ?(annot = None) types =
    match types with
    | [] | [ _ ] -> assert false
    | [ type1; type2 ] -> T.or_ ~annot type1 type2
    | type_ :: types -> T.or_ ~annot type_ (loop types)
  in
  loop types ~annot