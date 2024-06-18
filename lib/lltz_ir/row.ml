open Core

type label = Label of string [@@ocaml.unboxed] [@@deriving sexp, equal, compare]

let pp_label ppf (Label s) = Format.fprintf ppf "%s" s

module T = struct
  (** A row is a tree where each node has an arbitrary
      amount of children (zero or more). *)
  type 'a t =
    | Node of 'a t list
    | Leaf of label option * 'a
  [@@deriving sexp, equal, compare]

  let return x = Leaf (None, x)

  let rec bind t ~f =
    match t with
    | Node ts -> Node (List.map ts ~f:(fun t -> bind t ~f))
    | Leaf (_label, x) -> f x
  ;;

  let map = `Define_using_bind
end

include T
include Monad.Make (T)

let rec pp ppa ppf t =
  match t with
  | Node ts ->
    Format.fprintf
      ppf
      "Node (%a)"
      Format.(pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ") (pp ppa))
      ts
  | Leaf (label, x) ->
    Format.fprintf ppf "Leaf (%a, %a)" Format.(pp_print_option pp_label) label ppa x
;;

module Context = struct
  type 'a t =
    | Hole of 'a
    | Node of 'a t list * 'a t * 'a t list
  [@@deriving sexp, equal, compare]
end

module Path = struct
  type t = Here of int list [@@ocaml.unboxed] [@@deriving sexp, equal, compare]
end
