(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

[@@@alert unstable
    "This interface may change in incompatible ways or disappear \
     in the future."]

(** Color schemes.

    [Color_scheme] provides functions to generate continuous and
    discrete color schemes to map quantitative or qualitative data to
    colors with good perceptual properties.

    {b Sources.} The algorithmic color schemes are from
    {{:http://dx.doi.org/10.1111/j.1467-8659.2008.01203.x}
    M. Wijffelaars et al.}, the qualitative schemes are by
    {{:http://colorbrewer2.org/}Cynthia Brewer} and
    {{:https://research.tableau.com/user/maureen-stone} Maureen
    Stone}. The
    {{:https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html}turbo scheme} is by Anton Mikhailov.

    {b TODO}
    {ul
    {- Add images.}
    {- Don't return arrays.}
    {- Review schemes}
    {- Review docs and names.}} *)

(** {1:sequential Sequential schemes}

    Sequential color schemes are for ordered scalar data. *)

val seq : ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float ->
  h:float -> unit -> (float -> Gg.color)
(** [seq a w s b c h ()] is a function mapping the unit interval \[[0;1]\]
    to colors with a continuous sequential scheme where [0] is
    the darkest color and [1] the lightest. The parameters are:
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
    data. *)

val seq_d : ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float ->
  h:float -> int -> Gg.color array
(** [seq_d a w s b c h n] is like {!seq} except it
      returns a discrete sequential scheme with [n] colors and
      [c] defaults to [min 0.88 (0.34 +. 0.06. * n)]. *)

(** {1:sequential_multi Sequential multihue} *)

val turbo : ?a:float -> unit -> float -> Gg.color
(** [turbo ~a t] maps the unit interval \[[0;1]\] to the
      {{:https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html}turbo color scheme}. *)

(** {1:diverging Diverging schemes}

    Diverging color schemes are for ordered scalar data with a
    defined midpoint (e.g. zero or the data average). *)

val div : ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float ->
  ?m:float -> h0:float -> h1:float -> unit -> (float -> Gg.color)
(** [div a w s b c m h0 h1 ()] is a function mapping the unit interval
    \[[0;1]\] to colors for a continuous diverging scheme with [0] returning
    the darkest color of [h0], and [1] the darkest color of [h1].
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
    {- [a] is the alpha component, defaults to [1.].}} *)

val div_d : ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float ->
  ?m:float -> h0:float -> h1:float -> int -> Gg.color array
(** [div_d a w s b c m h0 h1 n] is like {!div} except it returns a
    discrete diverging scheme with [n] colors and [c] defaults to
    [min 0.88 (1.0 - 0.06 *. (11 - ((n / 2) + 1)))]. *)

(** {1:qualitative Qualitative schemes} *)

type qual_fixed =
[ `Brewer_accent_8 | `Brewer_dark2_8 | `Brewer_paired_12
| `Brewer_pastel1_9 | `Brewer_pastel2_8 | `Brewer_set1_9
| `Brewer_set2_8 | `Brewer_set3_12 | `Tableau_10 | `Wijffelaars_17 ]
(** The type for qualitative color scheme with fixed colors. The
    suffix indicates the maximal number of colors in the scheme. *)

val qual_fixed_size : qual_fixed -> int
(** [qual_fixed_size q] is the maximal number of colors in [qf]. *)

val qual_fixed : ?a:float -> ?size:int -> qual_fixed -> Gg.color array
(** [qual_fixed size q] is fixed qualitative color scheme [q] with
      [size] colors (defaults to [qual_fixed_size q]) and alpha
      component [a] (defaults to [1]).

      Raises [Invalid_argument] if [size] is greater than
      [qual_fixed_size b]. *)

val qual_d : ?a:float -> ?eps:float -> ?r:float -> ?s:float -> ?b:float ->
  ?c:float -> int -> Gg.color array
(** [qual_d eps r s b c n] is a qualitative scheme with [n] colors. The
      parameters are:
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
      {- [a] is the alpha component, defaults to [1.].}} *)

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers

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
