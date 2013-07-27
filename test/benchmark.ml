(*---------------------------------------------------------------------------
   Copyright (c) 2013 Edwin Török. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% release %%VERSION%%
  --------------------------------------------------------------------------*)

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
   Copyright (c) 2013 Edwin Török. 
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of the Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)


