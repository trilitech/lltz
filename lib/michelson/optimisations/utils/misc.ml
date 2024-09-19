(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

let memoize ?clear_after f =
  let cache = Hashtbl.create 10 in
  let rec f' x =
    (match clear_after with
    | Some n when Hashtbl.length cache > n -> Hashtbl.clear cache
    | _ -> ());
    match Hashtbl.find_opt cache x with
    | None ->
        let y = f f' x in
        Hashtbl.replace cache x y;
        y
    | Some y -> y
  in
  f'

let with_buffer x =
  let b = Buffer.create 64 in
  x b;
  Buffer.contents b

let buffer_protect b p op cl x =
  if p
  then (
    Buffer.add_string b op;
    x ();
    Buffer.add_string b cl)
  else x ()

let buffer_concat b sep f xs =
  List.iteri
    (fun i x ->
      if i <> 0 then Buffer.add_string b sep;
      f x)
    xs
