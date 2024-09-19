module M = Michelson.Ast 
module T = M.Type

let tuple types =
  (* Right-comb encoding of tuple-types *)
  match types with
  | [] -> T.unit
  | [ type_ ] -> type_
  | types -> T.pair types
;;

let rec tuple_left types =
  (* Left-comb encoding of tuple-types *)
  match types with
  | [] -> T.unit
  | [ type_ ] -> type_
  | a::b::types -> tuple_left ((T.pair [a;b])::types)

let ors types =
  (* Right-comb encoding of or-types (not efficient, but cheap) *)
  let rec loop = function
    | [] | [ _ ] -> assert false
    | [ type1; type2 ] -> T.or_ type1 type2
    | type_ :: types -> T.or_ type_ (loop types)
  in
  loop types
;;