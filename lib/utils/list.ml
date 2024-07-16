(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)
include Stdlib.List

type 'a t = 'a list [@@deriving eq, ord, show]

let rnth xs = nth (rev xs)

let rnth_opt xs = nth_opt (rev xs)

let rec nth_rem n r = function
  | [] -> None
  | x :: xs when n = 0 -> Some (x, rev_append r xs)
  | x :: xs -> nth_rem (n - 1) (x :: r) xs

let nth_rem n xs = nth_rem n [] xs

let rec last = function
  | [] -> failwith "List.last"
  | [x] -> x
  | _ :: xs -> last xs

let last_opt =
  let rec f r = function
    | [] -> r
    | x :: xs -> f (Some x) xs
  in
  fun xs -> f None xs

let split_at_opt =
  let rec split_at acc i = function
    | xs when i = 0 -> Some (rev acc, xs)
    | x :: xs when i > 0 -> split_at (x :: acc) (i - 1) xs
    | _ -> None
  in
  fun i l -> split_at [] i l

let split_at ?(err = "split_at") i l =
  match split_at_opt i l with
  | Some l -> l
  | None -> failwith err

let rec take i = function
  | x :: xs when i > 0 -> x :: take (i - 1) xs
  | _ -> []

let rec drop i = function
  | _ :: xs when i > 0 -> drop (i - 1) xs
  | xs -> xs

let rdrop i xs = take (length xs - i) xs

let rec rtl r = function
  | [] -> failwith "rtl"
  | [_] -> rev r
  | x :: xs -> rtl (x :: r) xs

let rtl xs = rtl [] xs

let rec unsnoc r = function
  | [] -> failwith "unsnoc"
  | [x] -> (rev r, x)
  | x :: xs -> unsnoc (x :: r) xs

let unsnoc xs = unsnoc [] xs

let find_ix x =
  let rec ix i = function
    | [] -> None
    | x' :: xs -> if x = x' then Some i else ix (i + 1) xs
  in
  ix 0

let rec is_prefix eq xs ys =
  match (xs, ys) with
  | [], _ -> true
  | x :: xs, y :: ys -> eq x y && is_prefix eq xs ys
  | _ -> false

let is_suffix eq xs ys = is_prefix eq (rev xs) (rev ys)

let rec strip_common_prefix eq xs ys =
  match (xs, ys) with
  | x :: xs, y :: ys when eq x y -> strip_common_prefix eq xs ys
  | _ -> (xs, ys)

let strip_common_suffix eq xs ys =
  let xs', ys' = strip_common_prefix eq (rev xs) (rev ys) in
  (rev xs', rev ys')

let map2 ?err f xs ys =
  match err with
  | Some err when length xs <> length ys -> failwith ("map2: " ^ err)
  | _ -> map2 f xs ys

let map3 ?err f xs ys = map2 ?err ( @@ ) (map2 ?err f xs ys)

let rec find_some f = function
  | [] -> None
  | x :: xs -> (
      match f x with
      | Some y -> Some y
      | None -> find_some f xs)

let rec map_some f = function
  | [] -> []
  | x :: xs -> (
      match f x with
      | None -> map_some f xs
      | Some y -> y :: map_some f xs)

let somes xs = map_some (fun x -> x) xs

let intersperse sep =
  let rec prepend = function
    | [] -> []
    | x :: xs -> sep :: x :: prepend xs
  in
  function
  | [] -> []
  | x :: xs -> x :: prepend xs

let assoc_opt ?(equal = ( = )) key =
  let rec f = function
    | [] -> None
    | (k, v) :: xs -> if equal key k then Some v else f xs
  in
  f

let remove_assoc ?(equal = ( = )) key =
  let rec f = function
    | [] -> []
    | ((a, _) as pair) :: l -> if equal a key then l else pair :: f l
  in
  f

let assoc_exn ?(msg = "assoc_exn") k l =
  match assoc_opt k l with
  | None -> failwith msg
  | Some y -> y

let replicate i x = init i (fun _ -> x)

let pp_sep s pp =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "%s" s) pp

let buffer_sep sep pp buf = function
  | [] -> ()
  | x :: xs ->
      pp buf x;
      let rec pps = function
        | x :: xs ->
            sep buf;
            pp buf x;
            pps xs
        | [] -> ()
      in
      pps xs

let map f =
  let rec map r = function
    | [] -> rev r
    | a :: l -> map (f a :: r) l
  in
  map []

let mapi f =
  let rec mapi i r = function
    | [] -> rev r
    | a :: l -> mapi (i + 1) (f i a :: r) l
  in
  mapi 0 []
