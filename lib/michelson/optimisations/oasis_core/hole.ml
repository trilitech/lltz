(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

type 'a t =
  | Variable of VarId.t
  | Value of 'a
[@@deriving eq, ord, show {with_path = false}, map, fold, sexp]

let variable x = Variable x

let value x = Value x

let mk () = Variable (VarId.mk ())

let t_of_sexp f : Sexplib.Sexp.t -> _ = function
  | Atom "Variable" -> mk ()
  | x -> t_of_sexp f x

let get = function
  | Variable _ -> None
  | Value x -> Some x

let get_value x =
  match get x with
  | None -> failwith "Hole.get_value"
  | Some x -> x
