(*---------------------------------------------------------------------------
   Copyright (c) 2023 The gg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

[@@@alert unstable
    "This interface may change in incompatible ways in the future."]

(** Rectangular, finite, discrete 2D fields and isolines.

    A {e field} assigns values to each point of a domain. A {e domain}
    is a finite rectangular region of 2D space represented by a
    {!Gg.box2} value.

    The map of values is represented by a discrete, possibly
    non-uniform grid of values stored in a linear array. The first
    value in the array corresponds to the top-left point of the domain
    and the last value to the bottom-right point. This follows the
    usual convention for raster images (is it a good idea though ?
    {!Gg.Raster} did not). *)

open Gg

(** {1:fields Fields} *)

type 'a t
(** The type for fields with values of type ['a] *)

val make : dom:Box2.t -> iw:int -> ih:int -> 'a array -> 'a t
(** [make ~dom ~iw ~ih vs] is a field on domain [dom] represented
    by the grid [iw] × [ih] of values [vs]. Raises [Invalid_argument] if
    the size of [vs] is not [iw * ih]. *)

val of_fun : dom:Box2.t -> (p2 -> 'a) -> dpu:float -> 'a t
(** [of_fun ~dom f ~dpu] samples function [f] on the domain [dom]
    at [dpu] values per unit (“dots per unit”) in each direction.  *)

val of_fun' : ?iw:int -> ?ih:int -> dom:Box2.t -> (p2 -> 'a) -> 'a t
(** [of_fun ?iw ?ih ~dom f] samples function [f] on the domain [dom]
    according to the grid defined by [iw] x [ih].  If a grid parameter
    is missing it is derived from the other for to provide a uniform
    grid on [dom]. If none is provided a uniform grid is generated
    with 1000 samples along the largest dimension; you get between
    1000 and 1'000'000 samples depending on your aspect ratio. *)

(** {1:props Properties} *)

val values : 'a t -> 'a array
(** [values field] are the values of [field]. *)

(** {2:grid_space Grid space} *)

val iw : 'a t -> int
(** [iw field] is the width of the grid of [field]. *)

val ih : 'a t -> int
(** [ih field] is the height of the grid of [field]. *)

val isize : 'a t -> Size2.t
(** [isize field] is {!iw} and {!ih} as a size. *)

val valuei : 'a t -> xi:int -> yi:int -> 'a
(** [valuei field ~xi ~yi] is [(values field).((iw field) * yi +
    xi)]. The point [(0,0)] corresponds to top left corner of [dom
    field], and the point [(w-1,h-1)] to the bottom right one.  *)

(** {2:dom_space Domain space} *)

val dom : 'a t -> Box2.t
(** [dom field] is the 2D rectangular domain covered by [field]. *)

val dpu : 'a t -> Size2.t
(** [dpu field] is the horizontal and vertical resolution of [field]
    in values (dots) per unit of domain space. *)

(** {1:transforming Transforming} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f field] maps the values of [field] with [f]. *)

val mapi : (xi:int -> yi:int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi] is like {!map} but gives the grid coordinates to the
    mapping function. *)

val mapd : (P2.t -> 'a -> 'b) -> 'a t -> 'b t
(** [mapd] is like {!map} but gives the domain coordinates to
    the mapping function. *)

val map_values : ('a array -> 'b array) -> 'a t -> 'b t
(** [map_values f field] maps the values of [field] with [f].
    Raises [Invalid_argument] if the resulting array size differs. *)

(** {1:isolines Isolines}  *)

val isoline : ?smooth:bool -> float t -> float -> Gg__pgon2.t
(** [isoline f iso] is a polygon bounding the areas of [f] where
    the value of [f] is [>= iso]. The rings of the polygon have
    correct winding order. Values outside the field [f] are assumed
    to be [neg_infinity] (< [iso]); this ensures that all polygons
    have a finite surface in {!dom}.

    The {{:https://en.wikipedia.org/wiki/Contour_line}isoline} is
    computed with the
    {{:https://en.wikipedia.org/wiki/Marching_squares} marching
    squares algorithm}. If [smooth] is [true] linear interpolation is
    used to find out the iso value on square boundaries, otherwise the
    iso value is assumed to be in the middle.

    {b Note.} We could generalize that, we'd need a comparator and
    an interpolator. *)

(** {1:as_image As images} *)

type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type for bigbytes. *)

val to_rgba :
  ?srgb:bool -> 'a t -> color:('a -> Color.t) -> iw:int -> ih:int ->
  bigbytes -> unit
(** [to_rgba f color ~iw ~ih img] renders the field to [img] assumed
    to pixel data of size [iw] × [ih] with pixels in RGBA order and first
    index of [img] pointing on the top left pixel. If
    [srgb] is true colors are written in [sRGB] space otherwise the
    raw linear color is written.

    {b Note.} {{:https://en.wikipedia.org/wiki/Nearest-neighbor_interpolation}
    Nearest neighbor interpolation} is used to map the pixels. *)
