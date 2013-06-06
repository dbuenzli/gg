open Gcolorspace
open Gg

let print_dig n fmt v = Format.fprintf fmt "%.*f" n v
let digits = 7

let print_matrix name m =
  Format.fprintf Format.std_formatter "%s: @\n" name;
  M3.print_f (print_dig digits) Format.std_formatter m;
  Format.fprintf Format.std_formatter "@\n"

let () =
  let rgb2xyz_d50 = RGB.to_pcsxyz RGB.rec709 (Whitepoint.of_temperature 6504.)
  in
(*  let rgb2xyz_d50 = RGB.srgb_to_pcsxyz in*)
  print_matrix "sRGB->XYZ_D50" rgb2xyz_d50;

  let Whitepoint d50 = Whitepoint.d50 in
  let scale = M3.scale (V3.div (V3.v 1. 1. 1.) d50) in
  let m2 = M3.mul scale rgb2xyz_d50 in
  print_matrix "sRGB->XYZ_D50 / D50" m2; (* used in gg.ml for RGB->LAB *)

  let rgb2xyz_d65 = RGB.srgb_to_xyz_d65 in
  print_matrix "sRGB->XYZ_D65" rgb2xyz_d65;

  print_matrix "XYZ_D650->sRGB" (M3.inv rgb2xyz_d65);

  let Whitepoint d65 = Whitepoint.d65 in
  let xy = (V3.x d65) +. 15. *. (V3.y d65) +. 3. *. (V3.z d65) in
  let u'n = 4. *. (V3.x d65) /. xy and v'n = 9. *. (V3.y d65)  /. xy in
  Printf.printf "u'n = %.7f; v'n = %.7f\n" u'n v'n
;;
