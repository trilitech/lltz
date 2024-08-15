include Core

module M = Michelson.Ast 
module T = M.Type

open T

let tuple types =
  match types with
  | [] -> unit
  | [ type_ ] -> type_
  | types -> pair types
;;