(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Utils

module T = struct
  type t = {var_id : int} [@@deriving eq, ord, show {with_path = false}]

  let t_of_sexp x = {var_id = Sexplib.Std.int_of_sexp x}

  let sexp_of_t x = Sexplib.Std.sexp_of_int x.var_id
end

include T

let counter = ref 0

let mk () =
  incr counter;
  {var_id = !counter}

module Set = Set.Make (T)
module Map = Map.Make (T)
