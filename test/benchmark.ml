(*---------------------------------------------------------------------------
   Copyright (c) 2013 Edwin Török. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Bigarray
open Bench

let n = 10000

let frand () = Float.random ~min:0. ~len:1. ()
let srgb_data =
  let ba = Array2.create float64 c_layout n 3 in
  for i = 0 to (n-1) do
    ba.{i,0} <- frand ();
    ba.{i,1} <- frand ();
    ba.{i,2} <- frand ();
  done;
  ba

let rgb_data = Array2.create float64 c_layout n 3
let lab_data = Array2.create float64 c_layout n 3
let lch_data = Array2.create float64 c_layout n 3

let srgb_to_rgb () =
  for i = 0 to (n-1) do
    let r = Array2.unsafe_get srgb_data i 0
    and g = Array2.unsafe_get srgb_data i 1
    and b = Array2.unsafe_get srgb_data i 2 in
    let color = Color.of_srgb (V4.v r g b 1.) in
    Array2.unsafe_set rgb_data i 0 (V4.x color);
    Array2.unsafe_set rgb_data i 1 (V4.y color);
    Array2.unsafe_set rgb_data i 2 (V4.z color);
  done

let rgb_to_srgb () =
  for i = 0 to (n-1) do
    let r = Array2.unsafe_get rgb_data i 0
    and g = Array2.unsafe_get rgb_data i 1
    and b = Array2.unsafe_get rgb_data i 2 in
    let srgb = Color.to_srgb (V4.v r g b 1.) in
    Array2.unsafe_set srgb_data i 0 (V4.x srgb);
    Array2.unsafe_set srgb_data i 1 (V4.y srgb);
    Array2.unsafe_set srgb_data i 2 (V4.z srgb);
  done

let rgb_to_lab () =
  for i = 0 to (n-1) do
    let r = Array2.unsafe_get rgb_data i 0
    and g = Array2.unsafe_get rgb_data i 1
    and b = Array2.unsafe_get rgb_data i 2 in
    let lab = Color.to_lab (V4.v r g b 1.) in
    Array2.unsafe_set lab_data i 0 (V4.x lab);
    Array2.unsafe_set lab_data i 1 (V4.y lab);
    Array2.unsafe_set lab_data i 2 (V4.z lab);
  done

let rgb_to_lch_ab () =
  for i = 0 to (n-1) do
    let r = Array2.unsafe_get rgb_data i 0
    and g = Array2.unsafe_get rgb_data i 1
    and b = Array2.unsafe_get rgb_data i 2 in
    let lch_ab = Color.to_lch_ab (V4.v r g b 1.) in
    Array2.unsafe_set lch_data i 0 (V4.x lch_ab);
    Array2.unsafe_set lch_data i 1 (V4.y lch_ab);
    Array2.unsafe_set lch_data i 2 (V4.z lch_ab);
  done

let lch_ab_to_rgb () =
  for i = 0 to (n-1) do
    let l = Array2.unsafe_get lch_data i 0
    and c = Array2.unsafe_get lch_data i 1
    and h = Array2.unsafe_get lch_data i 2 in
    let color = Color.of_lch_ab (V4.v l c h 1.) in
    Array2.unsafe_set rgb_data i 0 (V4.x color);
    Array2.unsafe_set rgb_data i 1 (V4.y color);
    Array2.unsafe_set rgb_data i 2 (V4.z color);
  done

let lab_to_rgb () =
  for i = 0 to (n-1) do
    let l = Array2.unsafe_get lab_data i 0
    and a = Array2.unsafe_get lab_data i 1
    and b = Array2.unsafe_get lab_data i 2 in
    let color = Color.of_lch_ab (V4.v l a b 1.) in
    Array2.unsafe_set rgb_data i 0 (V4.x color);
    Array2.unsafe_set rgb_data i 1 (V4.y color);
    Array2.unsafe_set rgb_data i 2 (V4.z color);
  done

let print_result result =
  Printf.printf "%s: (%.3f, %.3f) ms\n" result.desc
    (result.mean.Bootstrap.lower *. 1e3)
    (result.mean.Bootstrap.upper *. 1e3)

let () =
  config.gc_between_tests <- true;
  config.verbose <- false;
  config.output <- [List.iter print_result];
  bench [
    "sRGB -> RGB", srgb_to_rgb;
    "RGB -> sRGB", rgb_to_srgb;
    "RGB -> LAB", rgb_to_lab;
    "RGB -> LCH_AB", rgb_to_lch_ab;
    "LCH_AB -> RGB", lch_ab_to_rgb;
    "LAB -> RGB", lab_to_rgb
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
