(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

let read_file path =
  let i = open_in path in
  let buffer = Buffer.create (in_channel_length i) in
  let size = 1024 in
  let b = Bytes.create size in
  let rec step () =
    let r = input i b 0 size in
    if r <> 0
    then (
      Buffer.add_subbytes buffer b 0 r;
      step ())
  in
  step ();
  close_in i;
  Buffer.contents buffer

let write_file path s =
  let o = open_out path in
  output_string o s;
  close_out o

let with_out path k =
  let o = open_out path in
  let e = try `Ok (k o) with e -> `Err e in
  close_out o;
  match e with
  | `Ok r -> r
  | `Err e -> raise e
