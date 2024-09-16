(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

module Smartml_literal = Oasis_literal
open Utils
open Utils.Misc
open Sexplib.Std

type t =
  | Int of string
  | String of string
  | Bytes of string
  | Primitive of {
        name : string
      ; annotations : string list
      ; arguments : t list
    }
  | Sequence of t list
[@@deriving eq, ord, show {with_path = false}, sexp]

let rec to_json : _ -> Utils.Misc.json = function
  | Int i -> J_record [("int", J_string i)]
  | String s -> J_record [("string", J_string s)]
  | Bytes s -> J_record [("bytes", J_string Hex.(show (of_string s)))]
  | Primitive {name; annotations; arguments} ->
      let entries = [] in
      let entries =
        if annotations = []
        then entries
        else
          ("annots", J_list (List.map (fun x -> J_string x) annotations))
          :: entries
      in
      let entries =
        if arguments = []
        then entries
        else ("args", J_list (List.map to_json arguments)) :: entries
      in
      J_record (("prim", J_string name) :: entries)
  | Sequence xs -> J_list (List.map to_json xs)

let pp_as_json ?margin ?max_indent () ppf x =
  Misc.pp_json_as_json ?margin ?max_indent () ppf (to_json x)

let unAnnot = function
  | [] -> ""
  | annots -> Printf.sprintf "(%s)" (String.concat ", " annots)

let rec pretty indent = function
  | Int i -> Printf.sprintf "%s%s;" indent i
  | String s -> Printf.sprintf "%s'%s';" indent s
  | Bytes s -> Printf.sprintf "%s0x%s;" indent Hex.(show (of_string s))
  | Primitive {name; annotations; arguments = []} ->
      Printf.sprintf "%s%s%s;" indent name (unAnnot annotations)
  | Primitive {name; annotations; arguments} ->
      Printf.sprintf "%s%s%s{\n%s\n%s};" indent name (unAnnot annotations)
        (String.concat "\n"
           (List.map (fun x -> pretty (indent ^ "  ") x) arguments))
        indent
  | Sequence [] -> Printf.sprintf "%sSeq{}" indent
  | Sequence l ->
      Printf.sprintf "%sSeq{\n%s\n%s};" indent
        (String.concat "\n" (List.map (pretty (indent ^ "  ")) l))
        indent

let left x = Primitive {name = "Left"; annotations = []; arguments = [x]}

let right x = Primitive {name = "Right"; annotations = []; arguments = [x]}

let annotName t = String.sub t 1 (String.length t - 1)

let extractAnnot default = function
  | n1 :: n2 :: _ ->
      if String.sub n1 0 1 = "%"
      then annotName n1
      else if String.sub n2 0 1 = "%"
      then annotName n2
      else annotName n1 ^ "_" ^ annotName n2
  | [n] -> annotName n
  | [] -> default

let identity x = failwith ("Not handled " ^ pretty "" x)

let unString = function
  | `String s -> s
  | _ -> assert false

let rec parse (json : Yojson.Safe.t) =
  match json with
  | `String s -> String s
  | `Int _ | `Intlit _ -> assert false
  | `Bool b ->
      Primitive
        {
          name = (if b then "true" else "false")
        ; annotations = []
        ; arguments = []
        }
  | `Tuple _ | `Variant _ | `Float _ -> assert false
  | `Null -> String "NULL"
  | `Assoc l -> (
      match l with
      | [("int", `String s)] -> Int s
      | [("string", `String s)] -> String s
      | [("bytes", `String s)] ->
          Bytes (Hex.to_string (`Hex s)) (* used in tzstats parsing *)
      | _ ->
          let name =
            match List.assoc_opt "prim" l with
            | Some (`String name) -> name
            | _ -> assert false
          in
          let annotations =
            match List.assoc_opt "annots" l with
            | Some (`List annots) -> List.map unString annots
            | _ -> []
          in
          let arguments =
            match List.assoc_opt "args" l with
            | Some (`List args) -> List.map parse args
            | _ -> []
          in
          Primitive {name; annotations; arguments})
  | `List l -> Sequence (List.map parse l)

let int s = Int s

let string s = String s

let bytes s = Bytes s

(* let chain_id s = String (Hex.(show (of_string s))) *)

let primitive name ?(annotations = []) arguments =
  Primitive {name; annotations; arguments}

let sequence l = Sequence l
