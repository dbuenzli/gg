(*---------------------------------------------------------------------------
   Copyright (c) 2013 Edwin Török. All rights reserved.
   SPDX-License-Identifier: ISC
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
