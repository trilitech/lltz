(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena *)
open Core

type t =
  | Int of string
  | String of string
  | Bytes of string
  | Primitive of
      { name : string
      ; annotations : string list
      ; arguments : t list
      }
  | Sequence of t list
[@@deriving eq, ord, show { with_path = false }, sexp_of]

let unAnnot = function
  | [] -> ""
  | annots -> Printf.sprintf "(%s)" (String.concat ~sep:", " annots)
;;

let rec pretty indent = function
  | Int i -> Printf.sprintf "%s%s;" indent i
  | String s -> Printf.sprintf "%s'%s';" indent s
  | Bytes s -> Printf.sprintf "%s0x%s;" indent Hex.(show (of_string s))
  | Primitive { name; annotations; arguments = [] } ->
    Printf.sprintf "%s%s%s;" indent name (unAnnot annotations)
  | Primitive { name; annotations; arguments } ->
    Printf.sprintf
      "%s%s%s{\n%s\n%s};"
      indent
      name
      (unAnnot annotations)
      (String.concat
         ~sep:"\n"
         (List.map ~f:(fun x -> pretty (indent ^ "  ") x) arguments))
      indent
  | Sequence [] -> Printf.sprintf "%sSeq{}" indent
  | Sequence l ->
    Printf.sprintf
      "%sSeq{\n%s\n%s};"
      indent
      (String.concat ~sep:"\n" (List.map ~f:(pretty (indent ^ "  ")) l))
      indent
;;

let int s = Int s
let string s = String s
let bytes s = Bytes s

let primitive name ?(annotations = []) arguments =
  Primitive { name; annotations; arguments }
;;

let sequence l = Sequence l
