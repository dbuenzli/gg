open Gcolorspace
open Gg

let print_dig n fmt v = Format.fprintf fmt "%.*f" n v
let digits = 7
let print_matrix name m =
  Format.fprintf Format.std_formatter "%s: @\n" name;
  M3.print_f (print_dig digits) Format.std_formatter m;
  Format.fprintf Format.std_formatter "@\n"

let print_matrix_and_inv name m =
  let m' = M3.map (Float.round_dfrac digits) m in
  print_matrix name m';
  (* invert the rounded matrix to reduce roundtrip errors *)
  print_matrix ("inv " ^ name) (M3.inv m')


let () =
  let rgb2xyz_d50 = RGB.srgb_to_pcsxyz in
  print_matrix "sRGB->XYZ_D50" rgb2xyz_d50;

  let Whitepoint d50 = Whitepoint.d50 in
  let scale = M3.scale (V3.div (V3.v 1. 1. 1.) d50) in
  let m2 = M3.mul scale rgb2xyz_d50 in
  print_matrix_and_inv "sRGB->XYZ_D50 / D50" m2;
  (* used in gg.ml for RGB<->LAB *)

  print_matrix_and_inv "sRGB->XYZ_D65" RGB.srgb_to_xyz_d65;
  (* used in gg.ml for RGB<->LUV *)

  let Whitepoint d65 = Whitepoint.d65 in
  let xy = (V3.x d65) +. 15. *. (V3.y d65) +. 3. *. (V3.z d65) in
  let u'n = 4. *. (V3.x d65) /. xy and v'n = 9. *. (V3.y d65)  /. xy in
  Printf.printf "u'n = %.7f; v'n = %.7f\n" u'n v'n
;;
