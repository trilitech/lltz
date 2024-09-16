(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Control

type 'a t =
  | Leaf of 'a
  | Node of 'a t * 'a t
[@@deriving eq, ord, show {with_path = false}, map, fold]

let rec t_of_sexp f : Sexplib.Sexp.t -> _ = function
  | List [leaf] -> Leaf (f leaf)
  | List [l1; l2] -> Node (t_of_sexp f l1, t_of_sexp f l2)
  | l -> failwith ("Binary_tree.t_of_sexp: " ^ Sexplib.Sexp.to_string l)

let rec sexp_of_t f : _ -> Sexplib.Sexp.t = function
  | Leaf leaf -> List [f leaf]
  | Node (l1, l2) -> List [sexp_of_t f l1; sexp_of_t f l2]

let rec cata l n = function
  | Leaf x -> l x
  | Node (t1, t2) -> n (cata l n t1) (cata l n t2)

let size t = cata (fun _ -> 1) ( + ) t

include Monad (struct
  type nonrec 'a t = 'a t

  let return x = Leaf x

  let map = map

  let rec apply = function
    | Leaf f -> fun x -> map f x
    | Node (f1, f2) -> fun x -> Node (apply f1 x, apply f2 x)

  let rec bind = function
    | Leaf x -> fun f -> f x
    | Node (t1, t2) -> fun f -> Node (bind t1 f, bind t2 f)
end)

let leaf x = Leaf x

let node t1 t2 = Node (t1, t2)

let rec to_list = function
  | Leaf x -> [x]
  | Node (t1, t2) -> to_list t1 @ to_list t2

let rec fold1 f = function
  | Leaf x -> x
  | Node (x, y) -> f (fold1 f x) (fold1 f y)

let rec map_accum f s = function
  | Leaf x ->
      let s, y = f s x in
      (s, Leaf y)
  | Node (x, y) ->
      let s, x = map_accum f s x in
      let s, y = map_accum f s y in
      (s, Node (x, y))

let rec right_comb = function
  | [] -> failwith "of_right_comb: empty list"
  | [x] -> Leaf x
  | x :: xs -> Node (Leaf x, right_comb xs)

let rec is_right_comb = function
  | Leaf _ -> true
  | Node (Leaf _, t) -> is_right_comb t
  | _ -> false

let print sep pp =
  let rec f protect ppf = function
    | Leaf x -> Format.fprintf ppf "%a" pp x
    | Node (r1, r2) ->
        let r ppf = Format.fprintf ppf "%a@,%t%a" (f true) r1 sep (f true) r2 in
        if protect
        then Format.fprintf ppf "@[<hv>( %t@ )@]" r
        else Format.fprintf ppf "@[<hv>%t@]" r
  in
  f false

let rec map_some f = function
  | Leaf x -> Option.map leaf (f x)
  | Node (t1, t2) -> (
      match (map_some f t1, map_some f t2) with
      | Some x, Some y -> Some (node x y)
      | Some x, None -> Some x
      | None, y -> y)

let filter f x = map_some (fun x -> if f x then Some x else None) x

let rec matches x y =
  let open Option in
  match (x, y) with
  | Leaf x, y -> Some (Leaf (x, y))
  | Node (t1, t2), Node (u1, u2) -> node <$> matches t1 u1 <*> matches t2 u2
  | _ -> None

let rec common t u =
  match (t, u) with
  | Node (t1, t2), Node (u1, u2) -> node (common t1 u1) (common t2 u2)
  | _ -> leaf (t, u)

let rec common3 t u v =
  match (t, u, v) with
  | Node (t1, t2), Node (u1, u2), Node (v1, v2) ->
      node (common3 t1 u1 v1) (common3 t2 u2 v2)
  | _ -> leaf (t, u, v)

let rec same_shape x y =
  match (x, y) with
  | Leaf _, Leaf _ -> true
  | Node (t1, t2), Node (u1, u2) -> same_shape t1 u1 && same_shape t2 u2
  | _ -> false

let rec map2 f x y =
  match (x, y) with
  | Leaf x, Leaf y -> Leaf (f x y)
  | Node (t1, t2), Node (u1, u2) -> Node (map2 f t1 u1, map2 f t2 u2)
  | _ -> failwith "Binary_tree.map2: differing shapes"

let rec for_all f = function
  | Leaf x -> f x
  | Node (t1, t2) -> for_all f t1 && for_all f t2

let rec exists f = function
  | Leaf x -> f x
  | Node (t1, t2) -> exists f t1 || exists f t2

module Traversable (A : APPLICATIVE) = struct
  open A

  let rec map f = function
    | Leaf x -> leaf <$> f x
    | Node (r1, r2) -> node <$> map f r1 <*> map f r2

  let rec sequence = function
    | Leaf x -> leaf <$> x
    | Node (r1, r2) -> node <$> sequence r1 <*> sequence r2
end

type 'a context =
  | Hole
  | Node_left of 'a context * 'a t
  | Node_right of 'a t * 'a context
[@@deriving eq, show {with_path = false}, map]

let rec find_leaf p = function
  | Leaf x -> if p x then Some (x, Hole) else None
  | Node (t1, t2) -> (
      match find_leaf p t1 with
      | Some (l, ctxt) -> Some (l, Node_left (ctxt, t2))
      | None -> (
          match find_leaf p t2 with
          | None -> None
          | Some (l, x) -> Some (l, Node_right (t1, x))))

let rec fill x = function
  | Hole -> x
  | Node_left (c, t) -> Node (fill x c, t)
  | Node_right (t, c) -> Node (t, fill x c)

let rec context_cata h l r = function
  | Hole -> h
  | Node_left (c, t) -> l (context_cata h l r c) t
  | Node_right (t, c) -> r t (context_cata h l r c)

module Context = struct
  type 'a t = 'a context [@@deriving eq, show {with_path = false}]

  include Functor (struct
    type nonrec 'a t = 'a t

    let map = map_context
  end)
end
