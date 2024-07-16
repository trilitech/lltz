(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
type t =
  | Yes
  | No
  | Maybe

val and_ : t -> t -> t

val or_ : t -> t -> t
