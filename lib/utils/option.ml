(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Control
include Stdlib.Option

type 'a t = 'a option [@@deriving eq, show {with_path = false}]

let cata x f = function
  | None -> x
  | Some x -> f x

let some x = Some x

include Monad (struct
  type nonrec 'a t = 'a t

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let return = some

  let apply f x =
    match (f, x) with
    | Some f, Some x -> Some (f x)
    | _ -> None

  let bind x f = cata None f x
end)

let default d = cata d id

let is_none = function
  | None -> true
  | Some _ -> false

let is_some = function
  | None -> false
  | Some _ -> true

let of_some ?msg = function
  | Some x -> x
  | None -> failwith ("Option.of_some" ^ cata "" (( ^ ) ": ") msg)

let or_ x y =
  match (x, y) with
  | (Some _ as x), _ -> x
  | _, y -> y

let if_none x y =
  match x with
  | Some _ as x -> x
  | None -> y ()
