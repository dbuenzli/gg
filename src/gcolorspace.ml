open Gg

type whitepoint = Whitepoint of v3
module Whitepoint = struct
  type t = whitepoint

  let v ~x ~y =
    Whitepoint (V3.v (x /. y) 1. ((1. -. x -. y) /. y))

  let to_xy (Whitepoint v) =
    let sum = (V3.x v) +. (V3.y v) +. (V3.z v) in
    let x = (V3.x v) /. sum
    and y = (V3.y v) /. sum in
    V2.v x y

  let of_temperature t =
    let t2 = t *. t in
    let t3 = t2 *. t in
    let x =
      if t < 4000. || t > 25000. then
        invalid_arg "Color temperature out of range"
      else if t >= 4000. && t <= 7000. then
        -4.6070  *. 1e9 /. t3 +.
          2.9678  *. 1e6 /. t2 +.
          0.09911 *. 1e3 /. t +.
          0.244063
      else
        -2.0064  *. 1e9 /. t3 +.
          1.9018  *. 1e6 /. t2 +.
          0.24748 *. 1e3 /. t +.
          0.237040 in
    let y = -3. *. x *. x +. 2.87 *. x -. 0.275 in
    v x y;;

  (* standard illuminants *)
  let d50 = Whitepoint (V3.v 0.9642 1.0 0.8249)
  (* CIE illuminant D50, according to ICC v4 specification and sRGB *)

  let d65 = Whitepoint (V3.v 0.95047 1.0 1.08883)
  (** CIE illuminant D65.
   *
   * Everyone seems to define this with different truncations.
   *
   * Bruce Lindbloom: X=0.95047, Y=1.0, Z=1.08883
   * We use this definition.
   *
   * x=0.31271 y=0.32902 => X=0.9504 Y=1.0 Z=1.0889
   * This is the definition used by http://www.color.org/chadtag.xalter
   * and sRGB_IEC61966-2-1*.icc
   *
   * Rec 709 and sRGB: x=0.3127 y=0.3290 -> 0.9505 1. 1.0891
   * This is the definition used by sRGB.icm
   *
   * sRGB.icc (LittleCMS): X=0.95015, Y=1.0000, Z=1.08826
   * This matches if you calculate D65 with a CCT of 6504 K
   *)

  let d93 = of_temperature 9305. (* 9300 * 1.4388/1.438 *)
  let f2 = v ~x:0.37208 ~y:0.37529 (* CIE illuminant F2 *)
  let d55 = v ~x:0.33242 ~y:0.34743 (* CIE illuminant D55 *)
  let a = v ~x:0.44757 ~y:0.40745 (* CIE illuminant A *)
  let e = Whitepoint (V3.v 1. 1. 1.) (* CIE illuminant E (Equi-Power) *)
  let f8 = v ~x:0.34588 ~y:0.35875 (* CIE illuminant F8 *)
end

module XYZ = struct
  type t = v3
  let of_xy x y = V3.v (x /. y) 1. ((1. -. x -. y) /. y)

  type cat = CAT of m3

  let bradford = CAT (M3.v
      0.8951   0.2664  (-0.1614)
    (-0.7502)  1.7135    0.0367
      0.0389 (-0.0685)   1.0296
  )

  (* The original CAT02 matrix.
   * It can produce negative XYZ values though,
   * there are 2 unofficial "corrected" matrixes
   * that don't producek negative colors but produce slightly
   * worse results overall.
   * So lets use the original matrix until CIE TC8-11 chooses another.
  *)
  let cat02 = CAT (M3.v
      0.7328  0.4296 (-0.1624)
    (-0.7036) 1.6975   0.0061
      0.0030  0.0136   0.9834
  )

  let chromatic_adapt (CAT ma) ~src:(Whitepoint src) ~dst:(Whitepoint dst) =
    let sv = V3.ltr ma src and dv = V3.ltr ma dst in
    let m = M3.scale (V3.div dv sv) in
    M3.mul (M3.mul (M3.inv ma) m) ma;;
end

module RGB = struct
  type primaries = {
    xr: float; yr: float;
    xg: float; yg: float;
    xb: float; yb: float
  }

  let rec709 = {
    xr = 0.64; yr = 0.33;
    xg = 0.30; yg = 0.60;
    xb = 0.15; yb = 0.06;
  }

  let to_xyz primaries (Whitepoint w) =
    (*
     * Find a matrix M such that:
     * |Xw|   |1|  
     * |Yw|=M*|1|; 
     * |Zw|   |1|  
     * |xr|      |1| |xg|      |0| |xb|      |0|
     * |yr|*Yr=M*|0|;|yg|*Yg=M*|1|;|yb|*Yb=M*|0|;
     * |zr|      |0| |zg|      |0| |zb|      |1|
     *                 |Yr 0  0|
     * Substitute M=M1*|0 Yg  0| and simplify by Yr,Yg,Yb respectively:
     *                 |0  0 Yr| 
     * |xr|    |1| |xg|    |0| |xb|    |0| 
     * |yr|=M1*|0|;|yg|=M1*|1|;|yb|=M1*|0| =>
     * |zr|    |0| |zg|    |0| |zb|    |1| 
     *    | xr xg xb |  |Xw|    |Yr|    |Yr|       |Xw|
     * M1=| yr yg yb |; |Yw|=M1*|Yg| => |Yg|=M1^-1*|Yw|
     *    | zr zg zb |  |Zw|    |Yb|    |Yb|       |Zw|
     *)
    let r = XYZ.of_xy primaries.xr primaries.yr
    and g = XYZ.of_xy primaries.xg primaries.yg
    and b = XYZ.of_xy primaries.xb primaries.yb in
    let m1 = M3.of_cols r g b in
    let y = V3.ltr (M3.inv m1) w in
    M3.mul m1 (M3.scale y)

  let to_pcsxyz ?(cat = XYZ.bradford) primaries wp =
    let adapt = XYZ.chromatic_adapt cat ~src:wp ~dst:Whitepoint.d50 in
    M3.mul adapt (to_xyz primaries wp)

  let srgb_to_xyz_d65 = to_xyz rec709 Whitepoint.d65
  let srgb_to_pcsxyz = to_pcsxyz rec709 Whitepoint.d65
end
