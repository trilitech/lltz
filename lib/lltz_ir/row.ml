open Import

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

let rec iter t ~f =
  match t with
  | Node ts -> List.iter ts ~f:(fun t -> iter t ~f)
  | Leaf (_, x) -> f x
;;

let rec fold t ~init ~f =
  match t with
  | Node ts -> List.fold ts ~init ~f:(fun acc t -> fold t ~init:acc ~f)
  | Leaf (_, x) -> f x init
;;

let rec fold_map t ~init ~f =
  match t with
  | Node ts ->
    let acc, ts = List.fold_map ts ~init ~f:(fun acc t -> fold_map t ~f ~init:acc) in
    acc, Node ts
  | Leaf (label, x) ->
    let x', acc = f x init in
    acc, Leaf (label, x')
;;

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

module Context = struct
  type 'a t =
    | Hole of 'a
    | Node of 'a T.t list * 'a T.t * 'a T.t list
  [@@deriving sexp, equal, compare]
end

module Path = struct
  type t = Here of int list [@@ocaml.unboxed] [@@deriving sexp, equal, compare]
end

module Traverse_builtins = struct
  open Traverse_builtins

  class ['ctx] map_with_context =
    object
      method row__t : 'a. ('ctx, 'a) T.map_with_context -> ('ctx, 'a t) T.map_with_context
          =
        fun mapper ctx t -> map t ~f:(mapper ctx)
    end

  class map =
    object
      method row__t : 'a. 'a T.map -> 'a t T.map = fun mapper t -> map t ~f:mapper
    end

  class iter =
    object
      method row__t : 'a. 'a T.iter -> 'a t T.iter = fun f t -> iter t ~f
    end

  class ['acc] fold =
    object
      method row__t : 'a. ('a, 'acc) T.fold -> ('a t, 'acc) T.fold =
        fun folder t init -> fold t ~f:folder ~init
    end

  class ['acc] fold_map =
    object
      method row__t : 'a. ('a, 'acc) T.fold_map -> ('a t, 'acc) T.fold_map =
        fun fold_mapper t init ->
          let acc, t = fold_map t ~f:fold_mapper ~init in
          t, acc
    end
end

let rec find_leaf row =
  match row with
  | Leaf (_, x) -> Some x
  | Node rows ->
    (match rows with
     | [] -> None
     | hd :: tl ->
       (match find_leaf hd with
        | Some x -> Some x
        | None -> find_leaf (Node tl)))
