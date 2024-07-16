(* Copyright 2022-2023 Morum LLC, 2019-2022 Smart Chain Arena LLC *)

let id x = x

let pair x y = (x, y)

let flip f x y = f y x

let map_fst f (x, y) = (f x, y)

let map_snd f (x, y) = (x, f y)

module type TYPE = sig
  type t
end

module type EQ = sig
  type t

  val equal : t -> t -> bool
end

module type ORD = sig
  type t

  val compare : t -> t -> int
end

module type SHOW = sig
  type t

  val show : t -> string

  val pp : Format.formatter -> t -> unit
end

module type TYPE1 = sig
  type 'a t
end

module type EQ1 = sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module type ORD1 = sig
  type 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

module type SHOW1 = sig
  type 'a t

  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type FUNCTOR_CORE = sig
  include TYPE1

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE_CORE = sig
  include FUNCTOR_CORE

  val return : 'a -> 'a t

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type MONAD_CORE = sig
  include APPLICATIVE_CORE

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type APPLICATIVE_SYNTAX = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module type MONAD_SYNTAX = sig
  type 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

module type FUNCTOR = sig
  include FUNCTOR_CORE

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  include APPLICATIVE_CORE

  include FUNCTOR with type 'a t := 'a t

  include APPLICATIVE_SYNTAX with type 'a t := 'a t

  module Applicative_syntax : APPLICATIVE_SYNTAX with type 'a t := 'a t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val void : 'a t -> unit t

  val when_ : bool -> unit t -> unit t

  val when_some : 'a option -> ('a -> unit t) -> unit t

  val unless : bool -> unit t -> unit t

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val map_option : ('a -> 'b t) -> 'a option -> 'b option t

  val map_list : ('a -> 'b t) -> 'a list -> 'b list t

  val iter_list : ('a -> unit t) -> 'a list -> unit t

  val map2_list : ('a -> 'b -> 'c t) -> 'a list -> 'b list -> 'c list t

  val iter2_list : ('a -> 'b -> unit t) -> 'a list -> 'b list -> unit t

  val sequence_list : 'a t list -> 'a list t

  val sequence_option : 'a t option -> 'a option t

  val sequence_fst : 'a t * 'b -> ('a * 'b) t

  val sequence_snd : 'a * 'b t -> ('a * 'b) t

  val sequence_pair : 'a t * 'b t -> ('a * 'b) t

  val mapM_fst : ('a -> 'c t) -> 'a * 'b -> ('c * 'b) t

  val mapM_snd : ('b -> 'c t) -> 'a * 'b -> ('a * 'c) t
end

module type MONAD = sig
  include MONAD_CORE

  include APPLICATIVE with type 'a t := 'a t

  include MONAD_SYNTAX with type 'a t := 'a t

  module Monad_syntax : MONAD_SYNTAX with type 'a t := 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >> ) : unit t -> 'a t -> 'a t

  val join : 'a t t -> 'a t

  val bind2 : ('a -> 'b -> 'c t) -> 'a t -> 'b t -> 'c t

  val unless : bool -> unit t -> unit t
end

module Functor (A : FUNCTOR_CORE) : FUNCTOR with type 'a t := 'a A.t = struct
  include A

  let ( <$> ) = map
end

module Applicative (A : APPLICATIVE_CORE) :
  APPLICATIVE with type 'a t := 'a A.t = struct
  include A
  include Functor (A)

  let ( <*> ) = apply

  let void x = map (fun _ -> ()) x

  let when_ c x = if c then x else return ()

  let when_some c f =
    match c with
    | None -> return ()
    | Some x -> f x

  let unless c x = if c then return () else x

  let map2 f x y = f <$> x <*> y

  module Applicative_syntax = struct
    let ( let+ ) x = flip map x

    let ( and+ ) x = map2 pair x
  end

  include Applicative_syntax

  let cons x xs = x :: xs

  let map_option f = function
    | None -> return None
    | Some x -> (fun y -> Some y) <$> f x

  let rec map_list f = function
    | [] -> return []
    | x :: xs ->
        let+ y = f x
        and+ ys = map_list f xs in
        y :: ys

  let rec map2_list f xs ys =
    match (xs, ys) with
    | [], [] -> return []
    | x :: xs, y :: ys -> cons <$> f x y <*> map2_list f xs ys
    | _ -> failwith "map2_list: differing lengths"

  let iter_list f xs = void (map_list f xs)

  let iter2_list f xs ys = void (map2_list f xs ys)

  let sequence_list xs = map_list id xs

  let sequence_option = function
    | None -> return None
    | Some x -> (fun x -> Some x) <$> x

  let sequence_fst (x, y) =
    let+ x = x in
    (x, y)

  let sequence_snd (x, y) =
    let+ y = y in
    (x, y)

  let sequence_pair (x, y) =
    let+ x = x
    and+ y = y in
    (x, y)

  let mapM_fst f (x, y) =
    let+ x = f x in
    (x, y)

  let mapM_snd f (x, y) =
    let+ y = f y in
    (x, y)
end

module Monad (M : MONAD_CORE) : MONAD with type 'a t := 'a M.t = struct
  include M
  include Applicative (M)

  let ( >>= ) = bind

  let ( >> ) x y = x >>= fun _ -> y

  let join x = x >>= id

  let bind2 f x y =
    x >>= fun x ->
    y >>= fun y -> f x y

  let unless c x = if c then return () else x

  module Monad_syntax = struct
    let ( let* ) = ( >>= )

    let ( and* ) x y =
      x >>= fun x ->
      y >>= fun y -> return (x, y)
  end

  include Monad_syntax
end

module type FIXPOINT = sig
  type 'a f

  type t = F of t f

  val unF : t -> t f
end

module type UNTIED = sig
  include FIXPOINT

  val cata : ('a f -> 'a) -> t -> 'a

  val para : ((t * 'a) f -> 'a) -> t -> 'a
end

module Fix (F : TYPE1) : FIXPOINT with type 'a f := 'a F.t = struct
  type t = F of t F.t

  let unF (F x) = x
end

module FixFUNCTOR (F : FUNCTOR_CORE) :
  UNTIED with type 'a f := 'a F.t and type t := Fix(F).t = struct
  include Fix (F)

  let rec cata f (F t) = f (F.map (cata f) t)

  let rec para f (F t) = (F t, f (F.map (para f) t))

  let para f e = snd (para f e)
end

module FixEQ (F : EQ1) : EQ with type t := Fix(F).t = struct
  open Fix (F)

  let rec equal (F x) (F y) = F.equal equal x y
end

module FixORD (F : ORD1) : ORD with type t := Fix(F).t = struct
  open Fix (F)

  let rec compare (F x) (F y) = F.compare compare x y
end

module FixSHOW (F : SHOW1) : SHOW with type t := Fix(F).t = struct
  open Fix (F)

  let rec pp ppf (F x) = F.pp pp ppf x

  let show (F x) = F.show pp x
end

module Either = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b

  let of_left = function
    | Left x -> Some x
    | Right _ -> None

  let of_right = function
    | Left _ -> None
    | Right x -> Some x

  let is_left = function
    | Left _ -> true
    | Right _ -> false

  let is_right = function
    | Left _ -> false
    | Right _ -> true
end

module Result = struct
  type 'a t = ('a, string) result
  [@@deriving eq, ord, show {with_path = false}, map]

  let ok x = Ok x

  let error x = Error x

  let cata ok err = function
    | Ok r -> ok r
    | Error x -> err x

  include Monad (struct
    type nonrec 'a t = 'a t

    let map = map

    let return x = Ok x

    let apply f x =
      match f with
      | Ok f -> (
          match x with
          | Ok x -> Ok (f x)
          | Error e -> Error e)
      | Error e -> Error e

    let bind x f =
      match x with
      | Ok x -> f x
      | Error e -> Error e
  end)

  (* Optimized version. *)
  let map_list f =
    let rec map r = function
      | [] -> return (List.rev r)
      | x :: xs -> (
          match f x with
          | Ok y -> map (y :: r) xs
          | Error _ as e -> e)
    in
    fun xs -> map [] xs

  (* Optimized version. *)
  let sequence_list =
    let rec sq r = function
      | [] -> return (List.rev r)
      | Ok x :: xs -> sq (x :: r) xs
      | (Error _ as e) :: _ -> e
    in
    fun xs -> sq [] xs

  let get_ok = function
    | Ok x -> Some x
    | Error _ -> None

  let get_ok_exn = function
    | Ok x -> x
    | Error e -> failwith e

  let is_ok = function
    | Ok _ -> true
    | Error _ -> false

  let is_error = function
    | Ok _ -> false
    | Error _ -> true

  let map_error f = function
    | Ok x -> Ok x
    | Error e -> Error (f e)

  let get_error = function
    | Ok _ -> None
    | Error e -> Some e
end

module State (T : TYPE) = struct
  type 'a t = T.t -> 'a * T.t

  include Monad (struct
    type nonrec 'a t = 'a t

    let map f x s = map_fst f (x s)

    let return x s = (x, s)

    let bind x f s =
      let x, s = x s in
      f x s

    let apply f x = bind f (fun f -> bind x (fun x -> return (f x)))
  end)

  let get s = (s, s)

  let set s _ = ((), s)

  let local s a =
    let* bak = get in
    let* _ = set s in
    let* x = a in
    let* _ = set bak in
    return x

  let modify f = get >>= fun s -> set (f s)

  let run f s = f s

  let exec f s = fst (run f s)
end

module Reader (T : TYPE) = struct
  type 'a t = T.t -> 'a

  include Monad (struct
    type nonrec 'a t = 'a t

    let map f x s = f (x s)

    let return x _ = x

    let bind x f s =
      let x = x s in
      f x s

    let apply f x = bind f (fun f -> bind x (fun x -> return (f x)))
  end)

  let ask s = s

  let local f x s = x (f s)

  let run f s = f s
end

module Writer (T : TYPE) = struct
  type 'a t = T.t list * 'a

  include Monad (struct
    type nonrec 'a t = 'a t

    let map f (a, x) = (a, f x)

    let return x = ([], x)

    let bind (a, x) f =
      let a', x = f x in
      (a' @ a, x)

    let apply f x = bind f (fun f -> bind x (fun x -> return (f x)))
  end)

  let write a = ([a], ())

  let run (a, x) = (List.rev a, x)
end

let print_list xs ppf =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
    (fun ppf x -> Format.fprintf ppf "%t" x)
    ppf xs

let pp_int ppf = Format.fprintf ppf "%d"

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

let eq3 x y z = x = y && y = z

let pp_no_breaks f () =
  let fs = Format.pp_get_formatter_out_functions f () in
  Format.pp_set_formatter_out_functions f
    {fs with out_newline = (fun () -> ()); out_indent = (fun _ -> ())}

type ('a, 'b) pair = 'a * 'b [@@deriving eq, show {with_path = false}, map]
