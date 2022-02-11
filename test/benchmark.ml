(*---------------------------------------------------------------------------
   Copyright (c) 2013 Edwin Török. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg

let rand_rgb () =
  let r = Float.random ~min:0. ~len:1. () in
  let g = Float.random ~min:0. ~len:1. () in
  let b = Float.random ~min:0. ~len:1. () in
  V4.v r g b 1.

let rand_lch_ab () =
  let l = Float.random ~min:0. ~len:100. () in
  let c = Float.random ~min:0. ~len:181.02 () in
  let h = Float.random ~min:0. ~len:Float.two_pi () in
  V4.v l c h 1.

let rand_lab () =
  let l = Float.random ~min:0. ~len:100. () in
  let a = Float.random ~min:(-128.) ~len:(127. +. 128.) () in
  let b = Float.random ~min:(-128.) ~len:(127. +. 128.) () in
  V4.v l a b 1.

let srgb_to_rgb () = Color.of_srgb (rand_rgb ())
let rgb_to_srgb () = Color.to_srgb (rand_rgb ())
let rgb_to_lab () = Color.to_lab (rand_rgb ())
let rgb_to_lch_ab () = Color.to_lch_ab (rand_rgb ())
let lch_ab_to_rgb () = Color.of_lch_ab (rand_lch_ab ())
let lab_to_rgb () = Color.of_lab (rand_lab ())

let () =
  Unmark_cli.main "gg" @@
  Unmark.[
    bench "sRGB -> RGB" srgb_to_rgb;
    bench "RGB -> sRGB" rgb_to_srgb;
    bench "RGB -> LAB" rgb_to_lab;
    bench "RGB -> LCH_AB" rgb_to_lch_ab;
    bench "LCH_AB -> RGB" lch_ab_to_rgb;
    bench "LAB -> RGB" lab_to_rgb;
  ]

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Edwin Török

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
