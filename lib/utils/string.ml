(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Sexplib.Std
include Stdlib.String

let take n x = sub x 0 n

let drop n x = sub x n (length x - n)

let is_prefix p x =
  let l = length p in
  l <= length x && take l x = p

let pp ppf = Format.fprintf ppf "%s"

let repeat s n = Array.fold_left ( ^ ) "" (Array.make n s)

let explode x =
  let out = ref [] in
  iter (fun c -> out := c :: !out) x;
  List.rev !out

let implode x =
  let x = Array.of_list x in
  init (Array.length x) (fun i -> x.(i))

include Data.Make (struct
  type t = string [@@deriving sexp]

  let compare = compare

  let pp = pp
end)
