(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* ColorBrewer Color Schemes are Copyright 2002 Cynthia Brewer, Mark Harrower
   and the Pennsylvania State University. Licensed under the Apache License
   version 2.0.
   See http://www.apache.org/licenses/LICENSE-2.0

   The fixed qualitative color scheme `Wijffelaars_17 and the generated color
   schemes are taken from:

   M. Wijffelaars, Synthesis of Color Palettes, Technische Universiteit
   Eindhoven, 2008.

   M. Wijfeelaars et al, Generating Color Palettes using Intuitive
   Parameters, Eurographics 2008.
   https://doi.org/10.1111/j.1467-8659.2008.01203.x

   The turbo color scheme is Copyright 2019 Google LLC. Licensed under
   the Apache License version 2.0.
   See http://www.apache.org/licenses/LICENSE-2.0
*)

open Gg

let str = Printf.sprintf
let err_scheme_size ssize s = str "scheme size %d exceeded (%d)" ssize s

let r_lch_uv_c = 179.0413773582776
let y_lch_uv = V3.v 97.1392672 107.0642904 1.4987619    (* yellow LCH_uv. *)

(* Quadratic interpolation *)

let quad a b0 b1 b2 t =
  let omt = 1. -. t in
  let c0 = omt *. omt in
  let c1 = 2. *. omt *. t in
  let c2 = t *. t in
  V3.(V4.v ((c0 *. (x b0)) +. (c1 *. (x b1)) +. (c2 *. (x b2)))
        ((c0 *. (y b0)) +. (c1 *. (y b1)) +. (c2 *. (y b2)))
        ((c0 *. (z b0)) +. (c1 *. (z b1)) +. (c2 *. (z b2)))
        a)

let inv_quad b0 b1 b2 v =
  let c = b0 -. (2. *. b1) +. b2 in
  let r = (b0 -. b1 +. sqrt ((b1 *. b1) -. (b0 *. b2) +. (c *. v))) /. c in
  r

(* Hue functions *)

let mod_hue h =
  let h = mod_float h Float.two_pi in
  if h < 0. then h +. Float.two_pi else h

let mix_hue h0 h1 t =          (* mixes hue using shortest path on circle. *)
  let d = (mod_hue (h1 -. h0 +. Float.pi)) -. Float.pi in
  mod_hue (h0 +. t *. d)

let diff_hue h0 h1 =
  let d = abs_float (h1 -. h0) in
  if d < Float.pi then d else Float.two_pi -. d

let rgb2xyz = M3.v
    0.4124564 0.3575761 0.1804375   (* TODO From Gg.Color. *)
    0.2126729 0.7151522 0.0721750
    0.0193339 0.1191920 0.9503041

let msc h =    (* most satured color of [h], see Wijffelaars 2008, p. 36. *)
  let u'n = 0.1978398 in                           (* TODO From Gg.Color. *)
  let v'n = 0.4683363 in                           (* TODO From Gg.Color. *)
  let h0 = 0.21247617651602310 in           (* LCH_uv hue of RGB 1. 0. 0. *)
  let h1 = 1.49876191584179419 in           (* LCH_uv hue of RGB 1. 1. 0. *)
  let h2 = 2.22919630798637458 in           (* LCH_uv hue of RGB 0. 1. 0. *)
  let h3 = 3.35406860819474817 in           (* LCH_uv hue of RGB 0. 1. 1. *)
  let h4 = 4.64035469555325797 in           (* LCH_uv hue of RGB 0. 0. 1. *)
  let h5 = 5.37078918024889251 in           (* LCH_uv hue of RGB 1. 0. 1. *)
  let r, s, t =
    if h0 <= h && h < h1 then 1, 2, 0 else
    if h1 <= h && h < h2 then 0, 2, 1 else
    if h2 <= h && h < h3 then 2, 0, 1 else
    if h3 <= h && h < h4 then 1, 0, 2 else
    if h4 <= h && h < h5 then 0, 1, 2 else
    if h5 <= h || h < h0 then 2, 1, 0 else
    assert false
  in
  let alpha = -. sin h in
  let beta = cos h in
  let f = alpha *. u'n +. beta *. v'n in
  let coeff p =
    let c0 = V3.(f *. ((x p) +. (15. *. (y p)) +. (3. *. (z p)))) in
    let c1 = V3.((4. *. alpha *. (x p)) +. (9. *. beta *. (y p))) in
    c0 -. c1
  in
  let rv = -. (coeff (M3.col t rgb2xyz)) /. (coeff (M3.col r rgb2xyz)) in
  let a = [|0.; 0.; 0.|] in
  a.(r) <- rv; a.(s) <- 0.; a.(t) <- 1.;
  match a with [|r; g; b|] -> Color.v r g b 1.0 | _ -> assert false

(* Saturation functions. *)

let max_s l h =
  let pmid = Color.to_lch_uv (msc h) in
  let pendL = if l <= V4.x pmid then 0. else 100. in
  let alpha = (l -. pendL) /. (V4.x pmid -. pendL) in
  alpha *. V4.y pmid

(* Sequential color schemes, see Wijffelaars 2008 p. 747 table 1. *)

let p2_multi_hue w s h =            (* see Wijfelaars 2008 p. 747 table 2. *)
  let pb = y_lch_uv in
  let p2l = (1. -. w) *. 100. +. (w *. V3.x pb) in
  let p2h = mix_hue h (V3.z pb) w in
  let p2s = min (max_s p2l p2h) (w *. s *. V3.y pb) in
  V3.v p2l p2s p2h

let seq ?(a = 1.) ?(w = 0.) ?(s = 0.6) ?(b = 0.75) ?(c = 0.88) ~h () =
  let p0 = V3.v 0. 0. h in
  let p1 = V3.of_v4 (Color.to_lch_uv (msc h)) in
  let p2 = if w > 0. then p2_multi_hue w s h else V3.v 100. 0. h in
  let q0 = V3.((1. -. s) * p0 + s * p1) in
  let q2 = V3.((1. -. s) * p2 + s * p1) in
  let q1 = V3.(0.5 * (q0 + q2)) in
  fun t ->
    let l t = 125. -. 125. *. (0.2 ** ((1. -. c) *. b +. t *. c)) in
    let t_ l =
      if l <= V3.x q1
      then 0.5 *. (inv_quad (V3.x p0) (V3.x q0) (V3.x q1) l)
      else
      if l <= V3.x p2 then
        0.5 *. (inv_quad (V3.x q1) (V3.x q2) (V3.x p2) l) +. 0.5
      else
      (* This can happen with w != 0., in which case inv_quad returns
             NaN, just return the max. *)
      1.
    in
    let cseq t =
      if t <= 0.5
      then quad a p0 q0 q1 (2. *. t)
      else quad a q1 q2 p2 (2. *. (t -. 0.5))
    in
    Color.clamp (Color.of_lch_uv (cseq (t_ (l t))))

let seq_d ?a ?w ?s ?b ?c ~h n =
  let c = match c with
  | None -> min 0.88 (0.34 +. (float n) *. 0.06)
  | Some c -> c
  in
  let seq = seq ?a ?w ?s ?b ~c ~h () in
  let max = float (n - 1) in
  Array.init n (fun i -> seq ((float i) /. max))


(* https://gist.github.com/mikhailov-work/0d177465a8151eb6ede1768d51d476c7 *)
let turbo ?(a = 1.0) () = fun t ->
  let[@inline] dot4 x0 y0 z0 w0 x1 y1 z1 w1 =
    (x0 *. x1) +. (y0 *. y1) +. (z0 *. z1) +. (w0 *. w1)
  in
  let[@inline] dot2 x0 y0 x1 y1 = (x0 *. x1) +. (y0 *. y1) in
  let r4_x = 0.13572138 and r4_y = 4.61539260
  and r4_z = -42.66032258 and r4_w = 132.13108234 in
  let g4_x = 0.09140261 and g4_y = 2.19418839
  and g4_z = 4.84296658 and g4_w = -14.18503333 in
  let b4_x = 0.10667330 and b4_y = 12.64194608
  and b4_z = -60.58204836 and b4_w = 110.36276771 in
  let r2_x = -152.94239396 and r2_y = 59.28637943 in
  let g2_x = 4.27729857 and g2_y = 2.82956604 in
  let b2_x = -89.90310912 and b2_y = 27.34824973 in
  let v4_x = 1.0 and v4_y = t and v4_z = t *. t in let v4_w = t *. v4_z in
  let v2_x = v4_z *. v4_z in
  let v2_y = v4_w *. v4_z in
  let r =
    (dot4 v4_x v4_y v4_z v4_w r4_x r4_y r4_z r4_w) +.
    (dot2 v2_x v2_y r2_x r2_y)
  in
  let g =
    (dot4 v4_x v4_y v4_z v4_w g4_x g4_y g4_z g4_w) +.
    (dot2 v2_x v2_y g2_x g2_y)
  in
  let b =
    (dot4 v4_x v4_y v4_z v4_w b4_x b4_y b4_z b4_w) +.
    (dot2 v2_x v2_y b2_x b2_y)
  in
  Color.v_srgb r g b ~a

(* Diverging color schemes, see Wijffelaars 2008 p. 53 table 1. *)

let div
    ?(a = 1.) ?(w = 0.) ?(s = 0.6) ?(b = 0.75) ?(c = 0.88) ?(m = 0.5)
    ~h0 ~h1 ()
  =
  let seq0 = seq ~a ~w ~s ~b ~c ~h:h0 () in
  let seq1 = seq ~a ~w ~s ~b ~c ~h:h1 () in
  let e2 = 2. *. abs_float (m -. 0.5) in
  let t' =
    if m < 0.5 then (fun t -> (t +. e2) /. (1. +. e2)) else
    fun t -> t /. (1. +. e2)
  in
  let cm =
    let c0 = Color.to_lch_uv (seq0 1.) in
    let c1 = Color.to_lch_uv (seq1 1.) in
    let l = 0.5 *. (V4.x c0 +. V4.x c1) in
    let h = V3.z y_lch_uv in
    let s =
      let smax = w *. 0.5 *. (V4.y c0 +. V4.y c1) in
      min (max_s l h) smax
    in
    Color.of_lch_uv (V4.v l s h a)
  in
  fun t ->
    let t' = t' t in
    if Float.equal_tol ~eps:1e-8 t' 0.5 then cm else
    if t' < 0.5 then seq0 (2. *. t') else seq1 (2. *. (1. -. t'))

let div_d ?a ?w ?s ?b ?c ?(m = 0.5) ~h0 ~h1 n =
  let c = match c with
  | None -> min 0.88 (1.0 -. 0.06 *. (11. -. (float (n / 2 + 1))))
  | Some c -> c
  in
  let m' = floor (2. *. m *. ((float n) -. 1.) +. 0.5) /. 2. in
  if mod_float (2. *. m') 2. = 0. then
    let max = float (n - 1) in
    let div = div ?a ?w ?s ?b ~c ~m:(m' /. max) ~h0 ~h1 () in
    Array.init n (fun i -> div ((float i) /. max))
  else
  let max = float n in
  let div = div ?a ?w ?s ?b ~c ~m:((m' +. 0.5) /. max) ~h0 ~h1 () in
  Array.init n
    (fun i ->
       let i = float i in
       if i < m' +. 0.5
       then div (i /. max)
       else div ((i +. 1.) /. max))

(* Qualitative color schemes *)

type qual_fixed =
  [ `Brewer_accent_8 | `Brewer_dark2_8 | `Brewer_paired_12
  | `Brewer_pastel1_9 | `Brewer_pastel2_8 | `Brewer_set1_9
  | `Brewer_set2_8 | `Brewer_set3_12 | `Tableau_10 | `Wijffelaars_17 ]

let qual_fixed_size = function
| `Brewer_accent_8 -> 8 | `Brewer_dark2_8 -> 8 | `Brewer_paired_12 -> 12
| `Brewer_pastel1_9 -> 9 | `Brewer_pastel2_8 -> 8 | `Brewer_set1_9 -> 9
| `Brewer_set2_8 -> 8 | `Brewer_set3_12 -> 12 | `Tableau_10 -> 10
| `Wijffelaars_17 -> 17

let rgb = Color.v_srgbi
let brewer_accent_8 = lazy (* What the hell are these lazy doing here ? *)
  [| rgb 127 201 127; rgb 190 174 212; rgb 253 192 134; rgb 255 255 153;
     rgb 56  108 176; rgb 240 2   127; rgb 191 91   23; rgb 102 102 102; |]

let brewer_dark2_8 = lazy
  [| rgb 27  158 119; rgb 217 95  2  ; rgb 117 112 179; rgb 231 41  138;
     rgb 102 166 30 ; rgb 230 171 2  ; rgb 166 118 29 ; rgb 102 102 102; |]

let brewer_paired_12 = lazy
  [| rgb 166 206 227; rgb 31  120 180; rgb 178 223 138; rgb 51  160 44 ;
       rgb 251 154 153; rgb 227 26  28 ; rgb 253 191 111; rgb 255 127 0  ;
     rgb 202 178 214; rgb 106 61  154; rgb 255 255 153; rgb 177 89  40 ; |]

let brewer_pastel1_9 = lazy
  [| rgb 251 180 174; rgb 179 205 227; rgb 204 235 197; rgb 222 203 228;
     rgb 254 217 166; rgb 255 255 204; rgb 229 216 189; rgb 253 218 236;
     rgb 242 242 242; |]

let brewer_pastel2_8 = lazy
  [| rgb 179 226 205; rgb 253 205 172; rgb 203 213 232; rgb 244 202 228;
     rgb 230 245 201; rgb 255 242 174; rgb 241 226 204; rgb 204 204 204; |]

let brewer_set1_9 = lazy
  [| rgb 228 26  28 ; rgb 55  126 184; rgb 77  175 74 ; rgb 152 78  163;
     rgb 255 127 0  ; rgb 255 255 51 ; rgb 166 86  40 ; rgb 247 129 191;
     rgb 153 153 153; |]

let brewer_set2_8 = lazy
  [| rgb 102 194 165; rgb 252 141 98 ; rgb 141 160 203; rgb 231 138 195;
     rgb 166 216 84 ; rgb 255 217 47 ; rgb 229 196 148; rgb 179 179 179; |]

let brewer_set3_12 = lazy
  [| rgb 141 211 199; rgb 255 255 179; rgb 190 186 218; rgb 251 128 114;
     rgb 128 177 211; rgb 253 180 98 ; rgb 179 222 105; rgb 252 205 229;
     rgb 217 217 217; rgb 188 128 189; rgb 204 235 197; rgb 255 237 111; |]

let tableau_10 = lazy
  [| rgb 0x4e 0x79 0xa7; rgb 0xf2 0x8e 0x2b; rgb 0xe1 0x57 0x59;
     rgb 0x76 0xb7 0xb2; rgb 0x59 0xa1 0x4f; rgb 0xed 0xc9 0x48;
     rgb 0xb0 0x7a 0xa1; rgb 0xff 0x9d 0xa7; rgb 0x9c 0x75 0x5f;
     rgb 0xba 0xb0 0xab; |]

let wijffelaars_17 = lazy
  [| rgb 92  107 247; rgb 255 89  89 ; rgb 92  203 92 ; rgb 255 177 17 ;
     rgb 170 97  187; rgb 255 255 95 ; rgb 255 137 235; rgb 145 101 62 ;
     rgb 193 193 193; rgb 92  229 214; rgb 201 255 135; rgb 255 224 204;
     rgb 173  45  92; rgb 227 196 239; rgb 226 212 149; rgb 204 241 255;
     rgb  87 142  82; |]

let qual_fixed ?(a = 1.) ?size q =
  let qsize = qual_fixed_size q in
  let size = match size with
  | None -> qsize
  | Some s -> if s > qsize then invalid_arg (err_scheme_size qsize s); s
  in
  let scheme = match q with
  | `Brewer_accent_8 -> brewer_accent_8
  | `Brewer_dark2_8 -> brewer_dark2_8
  | `Brewer_paired_12 -> brewer_paired_12
  | `Brewer_pastel1_9 -> brewer_pastel1_9
  | `Brewer_pastel2_8 -> brewer_pastel2_8
  | `Brewer_set1_9 -> brewer_set1_9
  | `Brewer_set2_8 -> brewer_set2_8
  | `Brewer_set3_12 -> brewer_set3_12
  | `Tableau_10 -> tableau_10
  | `Wijffelaars_17 -> wijffelaars_17
  in
  let colors = Array.sub (Lazy.force scheme) 0 size in
  if a = 1. then colors else Array.map (fun c -> Color.with_a c a) colors

let qual ?(a = 1.) ?(eps = 0.) ?(r = 1.) ?(s = 0.5) ?(b = 1.) ?(c = 0.5) () =
  fun t ->                                 (* see Wijffelaars 2008, p. 65. *)
  let h = mod_hue ((V3.z y_lch_uv) +. Float.two_pi *. (eps +. t *. r)) in
  let alpha = (diff_hue h (V3.z y_lch_uv)) /. Float.two_pi in
  let l0 = b *. V3.x y_lch_uv in
  let l1 = (1. -. c) *. l0 in
  let l = (1. -. alpha) *. l0 +. alpha *. l1 in
  let s = min (max_s l h) (s *. r_lch_uv_c) in
  Color.clamp (Color.of_lch_uv (V4.v l s h a))

let qual_d ?a ?eps ?r ?s ?b ?c n =
  let qual = qual ?a ?eps ?r ?s ?b ?c () in
  let max = float n in
  Array.init n (fun i -> qual ((float i) /. max))

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
