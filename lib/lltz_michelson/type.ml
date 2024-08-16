include Core

module M = Michelson.Ast 
module T = M.Type

open T

let tuple types =
  (* Right-comb encoding of tuple-types *)
  match types with
  | [] -> unit
  | [ type_ ] -> type_
  | types -> pair types
;;

let ors types =
  (* Right-comb encoding of or-types (not efficient, but cheap) *)
  let rec loop = function
    | [] | [ _ ] -> assert false
    | [ type1; type2 ] -> or_ type1 type2
    | type_ :: types -> or_ type_ (loop types)
  in
  loop types
;;