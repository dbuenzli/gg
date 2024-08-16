(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

[@@@alert unstable
    "This interface may change in incompatible ways in the future."]

(** Color schemes.

    This module provides functions to generate continuous and discrete
    color schemes that map quantitative or qualitative data to colors
    with good perceptual properties. *)

(** {1:schemes Schemes} *)

type continuous = float -> Gg.color
(** The type for continuous schemes. A continuous scheme maps
    the unit interval \[[0.];[1.]\] to colors.
    Values outside the interval yield undefined results. *)

type discrete = int -> Gg.color
(** The type for discrete schemes. A discrete scheme maps an integer
    interval \[[0];[max]\] to colors,  with [max] depending on the scheme.
    Values outside the interval raise [Invalid_argument]. *)

(** {1:sequential Sequential}

    Sequential schemes are for ordered scalar data. *)

val sequential_wijffelaars :
  ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float -> h:float ->
  unit -> continuous
(** [seq_wijffelaars ~a ~w ~s ~b ~c ~h ()] is a sequential scheme
    where [0.] is the darkest color and [1.] the lightest. The
    parameters are:
    {ul
    {- [h] in \[[0;2pi]\] the main hue, the overall color.}
    {- [w] in \[[0;1]\] is the hue warmth for a multi-hue scheme,
       defaults to [0] (single-hue scheme). Augmenting [w] adds
       yellow which makes the scheme warmer.}
    {- [s] in \[[0;1]\] is saturation, the overall colorfullness,
       defaults to [0.6].}
    {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to
       [0.75].}
    {- [c] in \[[0;1]\] is contrast, the lightness difference
       between the darkest and the ligthest colors of the scheme,
       defaults to [0.88].}
    {- [a] is the alpha component, defaults to [1.].}}

    {b Note.} For equal [b], [c] and [w = 0], sequential schemes
    with different hues [h] have the same lightness. This can be
    used to generate multiple sequential schemes for multivariate
    data.

    This implements the sequential schemes described by
    {{:http://dx.doi.org/10.1111/j.1467-8659.2008.01203.x}
    M. Wijffelaars et al.}. *)

val sequential_wijffelaars' :
  ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float -> h:float ->
  size:int -> unit -> discrete
(** [sequential_wijffelaars' ~a ~w ~s ~b ~c ~h ~size] is like
    {!sequential_wijffelaars} except it returns a discrete sequential
    scheme with [size] colors and [c] defaults to [min 0.88 (0.34
    +. 0.06. * n)]. *)

(** {2:sequential_multi Multihue} *)

val sequential_turbo : ?a:float -> unit -> continuous
(** [sequential_turbo ()] is the
    {{:https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html}
    turbo} sequential scheme by Anton Mikhailov with alpha component
    [a] (defaults to [1.]). *)

val sequential_magma : ?a:float -> unit -> continuous
(** [sequential_magma ()] is the
    {{:https://bids.github.io/colormap/}plasma} color map
    by Stéfan van der Walt and Nathaniel Smith. with alpha component
    [a] (defaults to [1.]). *)

val sequential_inferno : ?a:float -> unit -> continuous
(** [sequential_inferno ()] is the
    {{:https://bids.github.io/colormap/}plasma} color map
    by Stéfan van der Walt and Nathaniel Smith. with alpha component
    [a] (defaults to [1.]). *)

val sequential_plasma : ?a:float -> unit -> continuous
(** [sequential_viridis ()] is the
    {{:https://bids.github.io/colormap/}plasma} color map
    by Stéfan van der Walt and Nathaniel Smith. with alpha component
    [a] (defaults to [1.]). *)

val sequential_viridis : ?a:float -> unit -> continuous
(** [sequential_viridis ()] is the
    {{:https://bids.github.io/colormap/}viridis} color map
    by Stéfan van der Walt, Nathaniel Smith and Eric Firing with alpha component
    [a] (defaults to [1.]). *)

(** {1:diverging Diverging}

    Diverging schemes are for ordered scalar data with a defined
    midpoint, like zero or the data average. *)

val diverging_wijffelaars :
  ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float ->
  ?m:float -> h0:float -> h1:float -> unit -> continuous
(** [diverging_wijffelaars ~a ~w ~s ~b ~c ~m ~h0 ~h1 ()] is a
    diverging scheme with [0.] returning the darkest color
    of [h0], and [1.] the darkest color of [h1].
    {ul
    {- [h0] in \[[0;2pi]\] is the hue, the overall color for lower values.}
    {- [h1] in \[[0;2pi]\] is the hue, the overall color for higher values.}
    {- [w] in \[[0;1]\] is the hue warmth for a multi-hue scheme,
         defaults to [0] (single-hue scheme). Augmenting [w] adds
         yellow which makes the scheme warmer.}
    {- [s] in \[[0;1]\] is saturation, the overall colorfullness,
         defaults to [0.6].}
    {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to
         [0.75].}
    {- [c] in \[[0;1]\] is contrast, the lightness difference
       between the darkest and the ligthest colors of the scheme,
       defaults to [0.88].}
    {- [m] is the mid point position, defaults to [0.5].}
    {- [a] is the alpha component, defaults to [1.].}}

    This implements the diverging schemes described by
    {{:http://dx.doi.org/10.1111/j.1467-8659.2008.01203.x}
    M. Wijffelaars et al.}. *)

val diverging_wijffelaars' :
  ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float -> ?m:float ->
  h0:float -> h1:float -> size:int -> unit -> discrete
(** [diverging_wijffelaars'] is like {!diverging_wijffelaars}
    except it returns a discrete diverging scheme with [size] colors
    and [c] defaults to [min 0.88 (1.0 - 0.06 *. (11 - ((n / 2) + 1)))]. *)

(** {1:cyclic Cyclic} *)

val cyclic_sinebow : ?a:float -> unit -> continuous
(** [cylic_sinebow ()] is the sinebow cyclical scheme by
    {{:https://krazydad.com/tutorials/makecolors.php}Jim Bumgardner}
    and {{:http://basecase.org/env/on-rainbows}Charlie Loyd}. *)

(** {1:qualitative Qualitative}

    Qualitative schemes are for nominal or categorical data. *)

type qualitative =
[ `Brewer_accent_8 | `Brewer_dark2_8 | `Brewer_paired_12 | `Brewer_pastel1_9
| `Brewer_pastel2_8 | `Brewer_set1_9 | `Brewer_set2_8 | `Brewer_set3_12
| `Tableau_10 | `Wijffelaars_17 ]
(** The type for qualitative schemes. The
    suffix indicates the number of colors in the scheme.
    {ul
    {- The [`Brewer_*] schemes are
       {{:http://colorbrewer2.org/}colorbrewer} schemes by Cynthia Brewer.}
    {- The [`Tableau_10] scheme is by
      {{:https://research.tableau.com/user/maureen-stone}Maureen Stone}.}
    {- The [`Wijffelaars_17] scheme is by
       {{:https://research.tue.nl/en/studentTheses/synthesis-of-color-palettes}
       M. Wijffelaars}.}} *)

val qualitative_size : qualitative -> int
(** [qualitative_size q] is the number of colors in [q]. *)

val qualitative : ?a:float -> qualitative -> unit -> discrete
(** [qualitative q] is the qualitative scheme [q] with [qualitative_size q]
    colors and alpha component [a] (defaults to [1.]). *)

val qualitative_wijffelaars :
  ?a:float -> ?eps:float -> ?r:float -> ?s:float -> ?b:float ->
  ?c:float -> size:int -> unit -> discrete
(** [qualitative_wijffelaars ~a ~eps ~r ~s ~b ~c ~size ()] is a qualitative
    scheme with [size] colors. The parameters are:
      {ul
      {- [eps] in \[[0;1]\] is the hue shift, defines where the range of hues
         begin, defaults to [0] (yellow).}
      {- [r] in \[[0;1]\] is the used hue range proportion, defaults to [1].}
      {- [s] in \[[0;1]\] is saturation, the overall colorfullness,
         defaults to [0.5].}
      {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to
         [1].}
      {- [c] in \[[0;1]\] is contrast, the lightness difference
         between the darkest and the ligthest colors of the scheme,
         defaults to [0.5].}
      {- [a] is the alpha component, defaults to [1.].}}

    This implements the qualitative schemes described by
    {{:https://research.tue.nl/en/studentTheses/synthesis-of-color-palettes}
    M. Wijffelaars}. *)
