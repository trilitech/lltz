open Core

module T = struct
  (** A rose tree is a tree where each node has an arbitrary
      amount of children (zero or more). *)
  type 'a t =
    | Node of 'a t list
    | Leaf of 'a
  [@@deriving sexp, equal, compare]

  (** A rose forest is nothing more than a list of rose trees. *)
  type 'a forest = 'a t list [@@deriving sexp, equal, compare]

  let return x = Leaf x

  let rec bind t ~f =
    match t with
    | Node forest -> Node (List.map forest ~f:(fun t -> bind t ~f))
    | Leaf x -> f x
  ;;

  let map = `Define_using_bind
end

include T
include Monad.Make (T)

let rec pp ppa ppf t =
  match t with
  | Node forest -> Format.fprintf ppf "Node (%a)" (pp_forest ppa) forest
  | Leaf x -> Format.fprintf ppf "Leaf %a" ppa x

and pp_forest ppa ppf forest =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
    (pp ppa)
    ppf
    forest
;;

module Context = struct
  type 'a t =
    | Hole of 'a
    | Node of 'a forest * 'a t * 'a forest
  [@@deriving sexp, equal, compare]
end

module Path = struct
  type t = Here of int list [@@ocaml.unboxed] [@@deriving sexp, equal, compare]
end
