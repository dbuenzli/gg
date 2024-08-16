(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** 2D polygons.

    A polygon is defined by a list of {!Ring2.t}s. Polygon
    surface is determined by the even-odd rule. *)

open Gg

(** {1:polygons Polygons} *)

type t
(** The type for polygons. *)

val of_rings : Gg__ring2.t list -> t
(** [of_rings rs] is a polygon from the list of rings [rs]. *)

val empty : t
(** [empty] is an empty polygon. *)

(** {1:props Properties} *)

val rings : t -> Gg__ring2.t list
(** [rings p] are the rings of [p]. *)

val box : t -> Box2.t
(** [box p] is a bounding box for the polygon. *)

(** {1:predicates Predicates} *)

val is_empty : t -> bool
(** [is_empty p] is [true] iff [p] is empty. *)

(** {1:bool Boolean operations}

    Boolean operations are implemented using
    {{:https://doi.org/10.1016/j.advengsoft.2013.04.004}this
    algorithm}.

    Input polygons can be concave and self-interesting the only
    constraint is that no two ring edges should overlap in a {e
    single} input polygon. The orientation of the rings does not
    matter for the algorithm it determines holes using the even-odd
    rule: any ring included in an odd number of rings bounds a
    hole.

    Overlap between input polygons is supported. *)

type holes = int list list
(** The type for the rings holds, one list per ring in the
    order of {!fold_contours}. The list has the position of holes in
    the order of {!fold_contours}. FIXME forget about that and
    return correctly oriented polygons. *)

type bool_op_error =
[ `Edge_overlap (** The input polygons have overlapping edges *)
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
(* This is exposed for [pgon_debug.ml] we should remove that at
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

(** {1:traversals Traversals} *)

val fold_rings : (Gg__ring2.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_rings f p acc] folds on the rings of [p]. *)
