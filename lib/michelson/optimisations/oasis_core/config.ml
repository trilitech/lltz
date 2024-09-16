(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

open Utils
include Michelson_base.Protocol
open Sexplib.Std
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type exceptions =
  | MetadataId
  | FullDebug
  | Message
  | VerifyOrLine
  | DefaultLine
  | Line
  | DefaultUnit
  | Unit
[@@deriving eq, ord, show {with_path = false}, sexp, yojson]

type mode =
  | Native
  | Mockup
  | Ghostnet
[@@deriving eq, ord, show {with_path = false}, sexp, yojson]

type t = {
    simplify : bool
  ; simplify_via_michel : bool
  ; erase_comments : bool
  ; disable_dup_check : bool
  ; protocol : protocol
  ; exceptions : exceptions
  ; dump_michel : bool
  ; single_entrypoint_annotation : bool
  ; warn_unused : bool
  ; default_check_no_incoming_transfer : bool
  ; mode : mode
  ; output : string option
}
[@@deriving eq, ord, show {with_path = false}, sexp, yojson]

type bool_flag =
  | Disable_dup_check
  | Dump_michel
  | Erase_comments
  | Simplify
  | Simplify_via_michel
  | Single_entrypoint_annotation
  | Warn_unused
  | Default_check_no_incoming_transfer
[@@deriving eq, ord, show, sexp]

type flag =
  | Bool_Flag of bool_flag * bool
  | Exceptions of exceptions
  | Protocol of protocol
  | Output of string option
  | Mode of mode
[@@deriving eq, ord, show, sexp]

let parse_bool_flag = function
  | "simplify" -> Some Simplify
  | "simplify-via-michel" -> Some Simplify_via_michel
  | "erase-comments" -> Some Erase_comments
  | "disable-dup-check" -> Some Disable_dup_check
  | "dump-michel" -> Some Dump_michel
  | "single-entrypoint-annotation" -> Some Single_entrypoint_annotation
  | "single-entry-point-annotation" -> Some Single_entrypoint_annotation
  | "warn-unused" -> Some Warn_unused
  | "default-check-no-incoming-transfer" ->
      Some Default_check_no_incoming_transfer
  | _ -> None

let string_of_bool_flag = function
  | Simplify -> "simplify"
  | Simplify_via_michel -> "simplify-via-michel"
  | Erase_comments -> "erase-comments"
  | Disable_dup_check -> "disable-dup-check"
  | Dump_michel -> "dump-michel"
  | Single_entrypoint_annotation -> "single-entrypoint-annotation"
  | Warn_unused -> "warn-unused"
  | Default_check_no_incoming_transfer -> "default-check-no-incoming-transfer"

let parse_bool_flag flag =
  let b, flag =
    if String.sub flag 0 3 = "no-"
    then
      let flag = String.sub flag 3 (String.length flag - 3) in
      (false, parse_bool_flag flag)
    else (true, parse_bool_flag flag)
  in
  Option.map (fun bf -> Bool_Flag (bf, b)) flag

let protocol_of_string protocol =
  match String.lowercase_ascii protocol with
  | "nairobi" -> Nairobi
  | "oxford" -> Oxford
  | "paris" -> Paris
  | _ -> Printf.ksprintf failwith "Unknown protocol: %S" protocol

let string_of_protocol = function
  | Nairobi -> "nairobi"
  | Oxford -> "oxford"
  | Paris -> "paris"

let mode_of_string mode =
  match String.lowercase_ascii mode with
  | "native" -> Native
  | "mockup" -> Mockup
  | "ghostnet" -> Ghostnet
  | _ -> Printf.ksprintf failwith "Unknown mode: %S" mode

let string_of_mode = function
  | Native -> "native"
  | Mockup -> "mockup"
  | Ghostnet -> "ghostnet"

let exceptions_of_string exceptions =
  match String.lowercase_ascii exceptions with
  | "metadata-id" -> MetadataId
  | "full-debug" | "fulldebug" -> FullDebug
  | "debug-message" | "debugmessage" -> Message
  | "verify-or-line" | "verifyorline" -> VerifyOrLine
  | "default-line" | "defaultline" -> DefaultLine
  | "line" -> Line
  | "default-unit" | "defaultunit" -> DefaultUnit
  | "unit" -> Unit
  | _ -> Printf.ksprintf failwith "Unknown exception flag: %S" exceptions

let string_of_exceptions = function
  | MetadataId -> "metadata-id"
  | FullDebug -> "full-debug"
  | Message -> "debug-message"
  | VerifyOrLine -> "verify-or-line"
  | DefaultLine -> "default-line"
  | Line -> "line"
  | DefaultUnit -> "default-unit"
  | Unit -> "unit"

let parse_flag = function
  | ["mode"; mode] -> Some (Mode (mode_of_string mode))
  | ["protocol"; protocol] -> Some (Protocol (protocol_of_string protocol))
  | ["exceptions"; exceptions] ->
      Some (Exceptions (exceptions_of_string exceptions))
  | ["output"; output] -> Some (Output (Some output))
  | [bf] -> parse_bool_flag bf
  | _ -> None

let string_of_flag = function
  | Mode mode -> ["mode"; string_of_mode mode]
  | Protocol protocol -> ["protocol"; string_of_protocol protocol]
  | Exceptions exceptions -> ["exceptions"; string_of_exceptions exceptions]
  | Bool_Flag (f, x) -> [(if x then "" else "no-") ^ string_of_bool_flag f]
  | Output output -> (
      match output with
      | None -> []
      | Some d -> ["output"; d])

let string_of_t config =
  let flags =
    [
      Bool_Flag (Simplify, config.simplify)
    ; Bool_Flag (Simplify_via_michel, config.simplify_via_michel)
    ; Bool_Flag (Erase_comments, config.erase_comments)
    ; Bool_Flag (Disable_dup_check, config.disable_dup_check)
    ; Bool_Flag (Dump_michel, config.dump_michel)
    ; Bool_Flag
        (Single_entrypoint_annotation, config.single_entrypoint_annotation)
    ; Bool_Flag (Warn_unused, config.warn_unused)
    ; Bool_Flag
        ( Default_check_no_incoming_transfer
        , config.default_check_no_incoming_transfer )
    ; Mode config.mode
    ; Protocol config.protocol
    ; Exceptions config.exceptions
    ]
  in
  List.map string_of_flag flags

let flag_of_sexp =
  let open Sexplib.Sexp in
  let unAtom = function
    | Atom n -> n
    | _ -> failwith "unAtom"
  in
  function
  | List x -> (
      let x = List.map unAtom x in
      match parse_flag x with
      | Some x -> x
      | _ -> failwith ("invalid flag usage: " ^ String.concat ", " x))
  | x -> failwith ("import_flag: " ^ to_string x)

let default =
  {
    disable_dup_check = false
  ; dump_michel = false
  ; erase_comments = false
  ; simplify_via_michel = false
  ; simplify = true
  ; single_entrypoint_annotation = true
  ; exceptions = VerifyOrLine
  ; protocol = Paris
  ; warn_unused = true
  ; default_check_no_incoming_transfer = false
  ; mode = Native
  ; output = None
  }

let is_initial_flag = function
  | Bool_Flag
      ( ( Disable_dup_check
        | Dump_michel
        | Erase_comments
        | Simplify
        | Simplify_via_michel
        | Single_entrypoint_annotation
        | Warn_unused
        | Default_check_no_incoming_transfer )
      , _ )
  | Exceptions _ | Output _ | Mode _ -> false
  | Protocol _ -> true

let apply_flag config = function
  | Bool_Flag (bf, b) -> (
      match (bf, b) with
      | Simplify, simplify -> {config with simplify}
      | Simplify_via_michel, simplify_via_michel ->
          {config with simplify_via_michel}
      | Erase_comments, erase_comments -> {config with erase_comments}
      | Disable_dup_check, disable_dup_check -> {config with disable_dup_check}
      | Dump_michel, dump_michel -> {config with dump_michel}
      | Single_entrypoint_annotation, single_entrypoint_annotation ->
          {config with single_entrypoint_annotation}
      | Warn_unused, warn_unused -> {config with warn_unused}
      | Default_check_no_incoming_transfer, default_check_no_incoming_transfer
        -> {config with default_check_no_incoming_transfer})
  | Mode mode -> {config with mode}
  | Protocol protocol -> {config with protocol}
  | Exceptions exceptions -> {config with exceptions}
  | Output output -> {config with output}
