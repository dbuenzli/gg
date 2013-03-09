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
    let color = Color.of_srgba (V4.v r g b 1.) in
    Array2.unsafe_set rgb_data i 0 (V4.x color);
    Array2.unsafe_set rgb_data i 1 (V4.y color);
    Array2.unsafe_set rgb_data i 2 (V4.z color);
  done

let rgb_to_srgb () =
  for i = 0 to (n-1) do
    let r = Array2.unsafe_get rgb_data i 0
    and g = Array2.unsafe_get rgb_data i 1
    and b = Array2.unsafe_get rgb_data i 2 in
    let srgba = Color.to_srgba (V4.v r g b 1.) in
    Array2.unsafe_set srgb_data i 0 (V4.x srgba);
    Array2.unsafe_set srgb_data i 1 (V4.y srgba);
    Array2.unsafe_set srgb_data i 2 (V4.z srgba);
  done

let rgb_to_lab () =
  for i = 0 to (n-1) do
    let r = Array2.unsafe_get rgb_data i 0
    and g = Array2.unsafe_get rgb_data i 1
    and b = Array2.unsafe_get rgb_data i 2 in
    let laba = Color.to_laba (V4.v r g b 1.) in
    Array2.unsafe_set lab_data i 0 (V4.x laba);
    Array2.unsafe_set lab_data i 1 (V4.y laba);
    Array2.unsafe_set lab_data i 2 (V4.z laba);
  done

let rgb_to_lch () =
  for i = 0 to (n-1) do
    let r = Array2.unsafe_get rgb_data i 0
    and g = Array2.unsafe_get rgb_data i 1
    and b = Array2.unsafe_get rgb_data i 2 in
    let lcha = Color.to_lcha (V4.v r g b 1.) in
    Array2.unsafe_set lch_data i 0 (V4.x lcha);
    Array2.unsafe_set lch_data i 1 (V4.y lcha);
    Array2.unsafe_set lch_data i 2 (V4.z lcha);
  done

let lch_to_rgb () =
  for i = 0 to (n-1) do
    let l = Array2.unsafe_get lch_data i 0
    and c = Array2.unsafe_get lch_data i 1
    and h = Array2.unsafe_get lch_data i 2 in
    let color = Color.of_lcha (V4.v l c h 1.) in
    Array2.unsafe_set rgb_data i 0 (V4.x color);
    Array2.unsafe_set rgb_data i 1 (V4.y color);
    Array2.unsafe_set rgb_data i 2 (V4.z color);
  done

let lab_to_rgb () =
  for i = 0 to (n-1) do
    let l = Array2.unsafe_get lab_data i 0
    and c = Array2.unsafe_get lab_data i 1
    and h = Array2.unsafe_get lab_data i 2 in
    let color = Color.of_lcha (V4.v l c h 1.) in
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
    "RGB -> LCH", rgb_to_lch;
    "LCH -> RGB", lch_to_rgb;
    "LAB -> RGB", lab_to_rgb
  ]
