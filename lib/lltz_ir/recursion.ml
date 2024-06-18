module type Functor = sig
  type 'a t [@@deriving sexp, equal, compare]

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

(* Recursion logic can be factored out here. *)
(* Bacause of the decoration there's it no added boilerplate for recursion schemes so the *)
(* recursive type is identical to what wo would make if used a single recursive definition *)
module Fixpoint (F : Functor) = struct
  type 'deco t = { deco : 'deco; inner : 'deco t F.t }
  [@@deriving sexp, equal, compare]

  let rec map { deco; inner } ~f =
    { deco = f deco; inner = F.map inner ~f:(map ~f) }

  let extract { deco; _ } = deco

  let rec duplicate ({ inner; _ } as t) =
    { deco = t; inner = F.map ~f:duplicate inner }

  let rec extend ({ inner; _ } as t) ~f =
    { deco = f t; inner = F.map ~f:(extend ~f) inner }

  let unwrap { inner; _ } = inner

  (* catamorphism with added decorator parameter *)
  let rec fold { deco; inner } ~f = f deco (F.map inner ~f:(fold ~f))

  let rec unfold inner ~f =
    let deco, inner = f inner in
    { deco; inner = F.map inner ~f:(unfold ~f) }

  let rec redecorate { deco; inner } ~f =
    let inner = F.map ~f:(redecorate ~f) inner in
    let deco = f deco inner in
    { deco; inner }
end
