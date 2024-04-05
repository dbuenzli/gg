(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** 2D linear rings.

    Linear rings are closed lists of straight segments. They may
    self-intersect.

    Rings are oriented using the right-hand rule. Counterclockwise
    rings bound a positive surface and clockwise ones a negative one
    (holes).

    Rings may self-interesect and be degenerate when made of less than
    three points. *)

open Gg

(** {1:linear_rings Linear rings} *)

type t
(** The type for linear rings. *)

val of_pts : P2.t list -> t
(** [of_pts pts] is a linear ring defined by points [pts]. Any two
    consecutive points of [pts] defines a segment of the ring. The
    last point is connected to the first one. *)

val empty : t
(** [empty] is an empty ring. *)

(** {1:props Properties} *)

val pts : t -> P2.t list
(** [pts r] is the list of points of [r]. See {!of_pts}. *)

val area : t -> float
(** [area r] is the signed area of the ring [r]. You may be suprised
    by results on self-intersecting rings. Returns [0.] on degenerate
    rings. *)

val centroid : t -> P2.t
(** [centroid r] is the {{:https://en.wikipedia.org/wiki/Centroid}
    centroid} of [r] *)

val box : t -> Box2.t
(** [box r] is the bounding box of [r]. This is {!Box2.empty} if
    {!is_empty}[ r] is [true]. *)

(** {1:predicates Predicates} *)

val is_empty : t -> bool
(** [is_empty r] is [true] iff [r] is empty, that is if has no points. *)

val mem : P2.t -> t -> bool
(** [mem pt r] is [true] iff [pt] is inside the ring [r]. *)

(** {1:transforming Transforming} *)

val swap_orientation : t -> t
(** [swap_orientation r] turns counterclockwise rings into clockwise
    ones and vice versa. *)

(** {1:traversals Traversals} *)

val fold_pts : (P2.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_pts f r acc] is the result of folding [f] with [acc] on the
    points of [r]. This is [acc] if [c] is empty. *)

val fold_segs : (P2.t -> P2.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_segs f r acc] is the result of folding [f] with [acc] on the
    segments of [r]. This is [acc] if [r] is empty, and calls [f p p]
    on degenerate singleton rings made of a single point. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
