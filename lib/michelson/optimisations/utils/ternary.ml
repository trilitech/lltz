(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
open Core

type t =
  | Yes
  | No
  | Maybe

let and_ x y =
  match (x, y) with
  | Yes, Yes -> Yes
  | No, _ | _, No -> No
  | _ -> Maybe

let or_ x y =
  match (x, y) with
  | Yes, _ | _, Yes -> Yes
  | Maybe, _ | _, Maybe -> Maybe
  | No, No -> No
