open Core

type ('k, 'v) t =
  | Leaf of 'v
  | Node of ('k * ('k, 'v) t) list
[@@deriving sexp, equal, compare]

let rec map t ~f =
  match t with
  | Leaf a -> Leaf (f a)
  | Node rts -> Node (List.map ~f:(fun (k, rt) -> k, map rt ~f) rts)
;;

module Path = struct
  type 'k t = Here of 'k list [@@ocaml.unboxed] [@@deriving sexp, equal, compare]
end
