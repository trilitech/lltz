(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Sexplib.Std
include Stdlib.Int

let pp ppf = Format.fprintf ppf "%d"

include Data.Make (struct
  type t = int [@@deriving sexp]

  let compare x y = compare x y

  let pp = pp
end)
