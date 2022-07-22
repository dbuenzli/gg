(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Unstable tools.

    These tools are unstable they can break between minor versions
    of the library. At some point they may get into {!Gg} or {!Gg_kit}
    or be removed entirely. *)

(** Polygons.

    {b TODO}

    - Not really happy about the naming scheme. Contours
      are the mathematical polygons. We could use Pgon and Multipgon
      but that doesn't seem to correspond what e.g. GeoJSON has.
      Also we should likely switch to Pgon2 in order to be consistent
      with Gg's conventions. *)
module Pgon : sig

  open Gg

  (** Contours.

      Contours are polygons made of a single closed list of straight segments.
      They may self-intersect. *)
  module Contour : sig

    type t
    (** The type for contours. *)

    val of_seg_pts : P2.t list -> t
    (** [of_seg_pts ps] is a contour defined by points [ps]. Any two
        consecutive points of [ps] defines a segment of the contour the
        last point is connected to the first one. *)

    val is_empty : t -> bool
    (** [is_empty c] is [true] iff [c] is empty. *)

    val fold_pts : (P2.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f c acc] is the result of folding [f] with [acc] on the points
        of [c]. This is [acc] if [c] is empty. *)

    val fold_segs : (P2.t -> P2.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_segs f c acc] is the result of folding [f] with [acc] on the
        segments of [c]. This is [acc] if [c] is empty, and calls [f p p] on
        degenerate singleton contours [[p]]. *)

    val centroid : t -> P2.t
    (** [centroid c] is the centroid of the contour. *)

    val mem : P2.t -> t -> bool
    (** [mem pt c] is [true] iff [pt] is inside the contour [c]. *)

    val area : t -> float
    (** [area c] is the signed area of the contour [c]. You may be
        suprised by results on self-intersecting contours. *)

    val box : t -> Box2.t
    (** [box c] is the bounding box of [c]. This is {!Box2.empty} if
        {!is_empty}[ c] is [true]. *)
  end

  type t
  (** The type for polygons. These polygons are made of several contours
      (polygons). Their surface is determined by the
      {{:https://en.wikipedia.org/wiki/Even%E2%80%93odd_rule}even-odd}
      rule that is does not depend on the orientation of their contours. *)

  val v : Contour.t list -> t
  (** [v cs] is a polygon from the list of contours. Polygon surface is
      determined by the even-odd rule. *)

  val empty : t
  (** [empty] is an empty polygon. *)

  val is_empty : t -> bool
  (** [is_empty p] is [true] iff [p] is empty. *)

  val fold_contours : (Contour.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_contours f p acc] folds on the contours of [p]. *)

  val box : t -> Box2.t
  (** [box p] is a bounding box for the polygon. *)

  (** {1:bool Boolean operations}

      Boolean operations are implemented using
      {{:https://doi.org/10.1016/j.advengsoft.2013.04.004}this
      algorithm}.

      Input polygons can be concave and self-interesting the only
      constraint is that no two contour edges should overlap in a
      {e single} input polygon. Overlap between input polygons
      is supported. *)

  type holes = int list list
  (** The type for the contour holds, one list per contour in the
      order of {!fold_contours}. The list has the position of holes in
      the order of {!fold_contours}. *)

  type bool_op_error =
  [ `Edge_overlap (** The input polygos have overlapping edges *)
  | `Topology_panic of string
    (** Internal error, please report the test case. *) ]

  type bool_op_result = (t * holes, (t * holes) * bool_op_error) result
  (** The type for boolean operation results. [Error (_, _, `Edge_overlap)]
      is returned if two contour edges overlap in a single input polygon.
      In that case there are two edges that overlap in either input polygon. In
      this case you should be suspicious about the result.

      The result polygon is a polygon whose surface can be determined
      both by the even oddrule and the non-zero winding rule
      (i.e. contours of holes are clockwise and other contours are
      counter clockwise).  This means that the {{!Contour.area}signed
      area} of contours can be used to compute the resulting surface.

      {b FIXME.}
      {ul
      {- The result can contain contours that self intersect
         seems it only happens with [xor]. This likely affects surface
         computation.}
      {- Holes are not computed for now. Simply interpret
         the result with the even-odd rule.}} *)

  val union : t -> t -> bool_op_result
  (** [union p0 p1] is the union of [p0] and [p1]. *)

  val diff : t -> t -> bool_op_result
  (** [diff p0 p1] is [p0] minus [p1]. *)

  val inter : t -> t -> bool_op_result
  (** [inter p0 p1] is the intersection of [p0] and [p1]. *)

  val xor : t -> t -> bool_op_result
  (** [xor p0 p1] is the exclusive union of [p0] and [p1]. *)

  (**/**)

  (* This is exposed for [pgon_debug.ml] we should removed that at
     some point. *)

  type polygon = Subject | Clipping (* left and right argument of bool ops *)
  type overlap = (* For overlap information see §7.2 of the first paper. *)
  | No | Not_contributing | Same_inout | Different_inout

  type bool_op = Union | Diff | Inter | Xor

  module Event : sig
    type t =
      { polygon : polygon;
        pt : P2.t;
        mutable other : t;
        mutable is_left : bool;
        mutable overlap : overlap;
        mutable inout : bool;
        mutable other_poly_inout : bool;
        mutable in_result : bool;
        mutable below_in_result : t;
        mutable resort : bool;
        mutable sort_index : int;
        mutable contour_id : int;
        mutable result_inout : bool; }

    val pp_dump : Format.formatter -> t -> unit
  end

  module Sweep : sig
    val debug_stepper : bool_op -> t -> t ->
      (unit ->
       (Event.t option * Event.t * Event.t option * Event.t list) option)

    val debug_result : ?filtered:bool -> bool_op -> t -> t -> Event.t list
  end
  (**/**)
end

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
