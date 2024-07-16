(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Type
open Primitive

(*val unify_types : ?tolerant:unit -> mtype -> mtype -> mtype Result.t*)

val type_prim0 : mtype prim0 -> mtype

val type_prim1 : mtype prim1 -> mtype * mtype

val type_prim2 : mtype prim2 -> ((mtype * mtype) * mtype) list

val type_prim3 : prim3 -> mtype * mtype * mtype * mtype

val is_packable_f : bool mtype_f -> bool

val is_packable : mtype -> bool
