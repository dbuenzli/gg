let parse_icc iccblob = failwith "TODO"
let write_iccv4 icc = failwith "TODO"

open Gg
open Raster
(* generic bigarray to bigarray colorspace conversion *)
let convert_ba src src_profile dst_profile =
  failwith "TODO"
(*
  let src_buffer = buffer src
  and src_format = format src in 
  let conv = Color.conversion ~src:src_profile ~dst:dst_profile in
  match src_buffer, format_dim src_format  with
  | `B_Uint8 buf, 2 ->
      (* WiP, not finished *)
      let h = h src_format
      and w = w src_format
      and w_skip = w_skip src_format
      and o = first src_format in
      let sample_size = sample_dim (sample_format src_format) in
      let tmp = Array.make w 0. in
      (* TODO: check bounds and use unsafe get for bigarray get/set *)
      for i = 0 to (h - 1) do
        let row = o + i * (w * sample_size + w_skip) in
        for j = 0 to (w-1) * sample_size do
          let pos = row + j in
          Array.unsafe_set tmp pos ((float buf.{pos}) /. 255.)
        done;
        let converted = conv tmp in
        for j = 0 to (w-1) * sample_size do
          let pos = row + j in
          (* TODO: set this in another buffer, NOT inplace *)
          buf.{pos} <- truncate ((Array.unsafe_get tmp pos) *. 255. +. 0.5)
        done;
      done;
      failwith "TODO"
  | _ ->
    failwith "TODO"
    *)
module Colorspace = struct

  let xyz_of_xy x y =
    V3.v (x /. y) 1. ((1. -. x -. y) /. y);;
  let xyz_of_v2 v2 =
    xyz_of_xy (V2.x v2) (V2.y v2);;

  type whitepoint = Whitepoint of v3
  module Whitepoint = struct
    type t = whitepoint
    let v ~x ~y =
      Whitepoint (xyz_of_xy x y);;

    let of_temperature ~t =
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

    let d65 = v ~x:0.3127 ~y:0.3290
    (* CIE illuminant D65, Rec 709 truncated coordinates *)

    let d93 = of_temperature 9305. (* 9300 * 1.4388/1.438 *)
    let f2 = v ~x:0.37208 ~y:0.37529 (* CIE illuminant F2 *)
    let d55 = v ~x:0.33242 ~y:0.34743 (* CIE illuminant D55 *)
    let a = v ~x:0.44757 ~y:0.40745 (* CIE illuminant A *)
    let e = Whitepoint (V3.v 1. 1. 1.) (* CIE illuminant E (Equi-Power) *)
    let f8 = v ~x:0.34588 ~y:0.35875 (* CIE illuminant F8 *)
  end

  module ICC = struct
    type t = unit (* TODO *)
    let of_string s = failwith "TODO"
    let dim v = failwith "TODO"
    type rendering_intent = [ `Perceptual | `Absolute_colorimetric | 
                              `Relative_colorimetric | `Saturation ]

    (* Are the 2 numbers the same if they were encoded
     * into s15Fixed16Number? *)
    let same_sf32 a b =
      (abs_float (a -. b)) < 1. /. 65536.;;

    type parametricCurve =
      [ `Gamma1 of float
      (** [Gamma g]
       * Y = X^g *)
      | `Gamma2 of float * float * float
      (** [Curve (g, a, b)]
       *  Y = (aX+b)^g when X>=-b/a
       *  Y = 0 when X <-b/a; CIE 122-1996 *)
      | `Gamma3 of float * float * float * float
      (** [Curve (g, a, b, c)]
       *  Y = (aX+b)^g+c when X >= -b/a
       *  Y = c when X < -b/a; IEC 61966-3 *)
      | `Gamma4 of float * float * float * float * float
      (** [Curve (g, a, b, c, d)]
       *  Y = (aX+b)^g when X >= d
       *  Y = cX when X < d;
       *  IEC 61966-2-1 (sRGB) *)
      | `Gamma5 of float * float * float * float * float * float * float
      (** [Curve (g, a, b, c, d, e, f)]
       *  Y = (aX+b)^g+e when X >= d
       *  Y = cX+f when X < d;
       *)
      ]

    let eval_curve c x = match c with
      | `Gamma1 g -> x ** g
      | `Gamma2 (g, a, b) ->
          if x >= -.b /. a then
            (a *. x +. b) ** g
          else 0.
      | `Gamma3 (g,a,b,c) ->
          if x >= -.b /. a then
            (a *. x +. b) ** g
          else c
      | `Gamma4 (g,a,b,c,d) ->
          if x >= d then
            (a *. x +. b) ** g
          else
            c *. x
      | `Gamma5 (g,a,b,c,d,e,f) ->
          if x >= d then
            (a *. x +. b) ** g +. e
          else
            c *. x +. f
      | `Custom f -> f x

    let parameters_of_curve = function
      | `Gamma1 g ->
         g, 1., 0., 0., 0., 0., 0.
      | `Gamma2 (g, a, b) ->
         g, a, b, 0., -. b /. a, 0., 0.
      | `Gamma3 (g, a, b, c) ->
         g, a, b, 0., -. b /. a, 0., c
      | `Gamma4 (g, a, b, c, d) ->
         g, a, b, c, d, 0., 0.
      | `Gamma5 (g, a, b, c, d, e, f) ->
         g, a, b, c, d, e, f

    let eval_type5 g a b c d e f x =
      if x >= d then
        (a *. x +. b) ** g +. e
      else
        c *. x +. f;;

    let inv curve =
      let g,a,b,c,d,e,f = parameters_of_curve curve in
      (** Y = (aX+b)^g+e when X >= d
       *  Y = cX + f when X < d
       *  ==>
       *  X = ((Y - e)^(1/g) - b)/a when Y >= split
       *  X = (Y - f)/c when Y < split
       *  ==>
       *  X = ((Y - e) * (1/a^g)) ^ (1/g) - b/a when Y >= split
       *  X = (1/c) * Y - f/c when Y < split
       *  ==>
       *  a' = a^-g
       *  b' = -e * a'
       *  c' = 1/c
       *  d' = c' * d + f'
       *  e' = -b/a
       *  f' = -f/c
       *  g' = 1/g
       *)
      let a' = a ** (-.g) in
      let b' = -. e *. a' in
      let c' = 1. /. c in
      let f' = -. f /. c in
      let d' = c *. d +. f in
      let e' = -. b /. a in
      let g' = 1. /. g in
      `Gamma5 (g', a', b', c', d', e', f');;

    (* ignore this for now, its not used *)
    (* create a curve that combines other 2 *)
    let compose_pcurve c1 c2 =
       (* X1 = (a1*X+b1)^g1+e1 when X >= d1
       *  X1 = c1*X+f1 when X < d1;
       *  Y = (a2*X1+b2)^g2+e2 when X >= d2
       *  Y = c2*X1+f2 when X < d2; *)
      (* bring to common representation *)
      let g1,a1,b1,c1,d1,e1,f1 = parameters_of_curve c1
      and g2,a2,b2,c2,d2,e2,f2 = parameters_of_curve c2 in
      let c1_split =
        eval_type5 g1 a1 b1 c1 d1 e1 f1 d1 in
      let split =
        if same_sf32 c1_split d2 || d2 = 0. then Some d1
        else if c1_split = 0. then
          let `Gamma5 (_, _, _, _, d, _, _) = inv (`Gamma5 (g2, a2, b2, c2, d2, e2, f2)) in
          Some d
        else None in
      match split, g1 = 1., g2 = 1. with
      | Some d, true, _ ->
          (* first curve is linear *)
          let a' = a2 *. a1
          and b' = a2 *. (b1 +. e1) +. b2
          and e' = e2
          and c' = c2 *. c1
          and f' = c2 *. f1 +. f2 in
          Some (`Gamma5 (g2, a', b', c', d, e', f'))
      | Some d, _, true ->
          (* second curve is linear *)
          let a2' = a2 ** (1. /. g1) in
          let a' = a1 *. a2'
          and b' = b1 *. a2'
          and e' = a2 *. e1 +. b2 +. e2
          and c' = c2 *. c1
          and f' = c2 *. f1 +. f2 in
          Some (`Gamma5 (g1, a', b', c', d, e', f'))
      | Some _, false, false -> None (* can't expand (a+b)^g *)
      | None, _, _ -> None (* more than one discontinuity point would be needed *)
    ;;

    let optimize_curve p =
      let g, a, b, c, d, e, f = parameters_of_curve p in
      if (same_sf32 a 1.) &&
         (same_sf32 b 0.) && (same_sf32 c 0.) && (same_sf32 d 0.) &&
         (same_sf32 e 0.) && (same_sf32 f 0.) then
           `Gamma1 g
      else if (same_sf32 d (-. b /. a)) &&
           (same_sf32 c 0.) && (same_sf32 e 0.) then begin
        if same_sf32 f 0. then
          `Gamma2 (g, a, b)
        else
          `Gamma3 (g, a, b, f)
      end else if same_sf32 e 0. && same_sf32 f 0. then
        `Gamma4 (g,a,b,c,d)
      else
        `Gamma5 (g, a, b, c, d, e, f)
    ;;

    (* Are these 2 XYZ numbers the same after encoding
     * to XYZNumber? *)
    let same_xyz (`XYZ a) (`XYZ b) =
      V3.equal_f same_sf32 a b;;
  end
  let to_icc _ = failwith "TODO"

  type sXYZ = [ `XYZ of Whitepoint.t ]
  type sLAB = [ `LAB of Whitepoint.t ]
  type sLUV = [ `LUV of Whitepoint.t ]
  type curve = [ ICC.parametricCurve | `Linear | `Custom of  float -> float ]
  type rgb_space = { r: v2; g: v2; b: v2; w: Whitepoint.t; f: curve }
  type sRGB = [ `RGB of rgb_space ]
  type sYCbCr = [ `YCbCr of rgb_space ]
  type sYxy = [ `Yxy of Whitepoint.t ]
  type sGray = [ `Gray of Whitepoint.t * curve ]
  type sHSV = [ `HSV ]
  type sHLS = [ `HLS ]
  type cmy_space
  type sCMY = [ `CMY of cmy_space ]
  type cmyk_space
  type sCMYK = [ `CMYK of cmyk_space ]
  type ciecam02_space = {
      la: float; yb: float;
      surround: [ `Average | `Dim | `Dark]
  }
  type sCAM02Jab = [ `CAM02Jab of ciecam02_space ]
  type sCAM02JCh = [ `CAM02JCh of ciecam02_space ]
  type sColors = [ `Colors of int ]
  type sIPT = [ `IPT ]
  type sLCH = [ `LCH of Whitepoint.t ]

  type space1 = sGray
  type space3 = [ sXYZ | sLAB | sLUV | sRGB | sYCbCr | sYxy | sHSV | sHLS | sCMY
  | sCAM02Jab | sCAM02JCh | sIPT | sLCH]
  type space4 = sCMYK
  type spacen = sColors
  type t = [ space1 | space3 | space4 | spacen ]

  type c1 = [ `V1 of float ]
  type c3 = [ `V3 of v3 ]
  type c4 = [ `V4 of v4 ]
  type cn = [ `Vn of float array ]
  type raw_sample = [ c1 | c3 | c4 | cn ]
  type ('a,'b) sample = ([< raw_sample] as 'b) * ([< t] as 'a)

  let of_sample (_, space) = space
  let sample_of_v1 (s1:[< space1]) v1 = `V1 v1, s1
  let sample_of_v3 (s3:[< space3]) v3 = `V3 v3, s3
  let sample_of_v4 (s4:[< space4]) v4 = `V4 v4, s4
  let sample_of_vn (sn:[< spacen]) vn = `Vn vn, sn



  module XYZ = struct
    type t = (sXYZ,c3) sample
    let x (`V3 v, _) = V3.x v
    let y (`V3 v, _) = V3.y v
    let z (`V3 v, _) = V3.z v
    let chromatic_adapt ma ~src:(Whitepoint src) ~dst:(Whitepoint dst) =
      let sv = V3.ltr ma src and dv = V3.ltr ma dst in
      let m = M3.scale (V3.v
        (V3.x dv/. V3.x sv) (V3.y dv /. V3.y sv) (V3.z dv/. V3.z sv)) in
      M3.mul (M3.mul (M3.inv ma) m) ma;;

    let bradford = M3.v
      0.8951   0.2664  (-0.1614)
     (-0.7502) 1.7135    0.0367
      0.0389 (-0.0685)   1.0296

    (* The original CAT02 matrix.
     * It can produce negative XYZ values though,
     * there are 2 unofficial "corrected" matrixes
     * that don't producek negative colors but produce slightly
     * worse results overall.
     * So lets use the original matrix until CIE TC8-11 chooses another.
     *)
    let cat02 = M3.v
      0.7328  0.4296 (-0.1624)
    (-0.7036) 1.6975   0.0061
      0.0030  0.0136   0.9834
  end
  module LAB = struct
    type t = (sLAB,c3) sample
    let l ((`V3 v, _):t) = V3.x v
    let a (`V3 v, _) = V3.y v
    let b (`V3 v, _) = V3.z v
    let eps = 216.0 /. 24389.0
    let k = 24389.0 /. 27.0
    let f v =
      if v > eps then
        v ** (1.0 /. 3.0)
    else
      (k *. v +. 16.0) /. 116.0;;

    let of_xyz (`XYZ (xyz, `Whitepoint w)) =
      let xr, yr, zr = V3.to_tuple w in
      let x, y, z = V3.to_tuple xyz in
      let fx = f (x /. xr) and fy = f (y /. yr) and fz = f (z /. zr) in
      let l = 116. *. fy -. 16.
      and a = 500. *. (fx -. fy)
      and b = 200. *. (fy -. fz) in
      `LAB (V3.v l a b), w;;

    let c v =
      let a = a v and b = b v in
      sqrt (a *. a +. b *. b);;
    let h v =
      let r = Float.deg_of_rad (atan2 (b v) (a v)) in
      (* r is [-pi,pi] now *)
      if r < 0. then 360. +. r else r;;
    (* h is [0, 360] *)

    let cie94_deltaE (`LAB v1) (`LAB v2) =
      let dL = (l v1) -. (l v2)
      and c1 = c v1 and c2 = c v2 in
      let dC = c1 -. c2
      and da = (a v1) -. (a v2)
      and db = (b v1) -. (b v2) in
      let dH2 = da *. da +. db *. db -. dC *. dC
      and sC = 1. +. 0.045 *. c1 (* k1 = graphic arts *)
      and sH = 1. +. 0.015 *. c2 (* k2 = graphic arts *) in
      sqrt (
        dL *. dL (*/ kL * sL = 1 *) +.
        (dC *. dC /. sC (* kC = 1 *)) +.
        (dH2 /. sH (* kH = 1 *))
      );;
  end
  module LCH = struct
    type t = (sLCH,c3) sample
    let l (`V3 v, _) = V3.x v
    let c (`V3 v, _) = V3.y v
    let h (`V3 v, _) = V3.z v
  end
  module LUV = struct
    type t = (sLUV,c3) sample
    let l (`V3 v, _) = V3.x v
    let u (`V3 v, _) = V3.y v
    let v (`V3 v, _) = V3.z v
  end
  module RGB = struct
    type t = (sRGB,c3) sample
    let r (`V3 v, _) = V3.x v
    let g (`V3 v, _) = V3.y v
    let b (`V3 v, _) = V3.z v

    let m (`RGB primaries) =
      (* TODO: handle curve! *)
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
    let r = xyz_of_v2 primaries.r
    and g = xyz_of_v2 primaries.g
    and b = xyz_of_v2 primaries.b
    and Whitepoint w = primaries.w in
    let m1 = M3.of_cols r g b in
    let y = V3.ltr (M3.inv m1) w in
    M3.mul m1 (M3.scale y);;
  end

  module SRGB = struct
    type t = (sRGB,c3) sample
    let r (`V3 v, _) = V3.x v
    let g (`V3 v, _) = V3.y v
    let b (`V3 v, _) = V3.z v

    let curve =
      let g = 1./.2.4 and a = 1.055 and b=0. and e = -0.055 and d = 0.0031308
      (* X' = (1.055 * X) ^ 1/2.4 when X >= 0.0031308 *)
      and c = 12.92 and f = 0. in
      (* X' = 12.92*X when X < 0.0031308 *)
      `Gamma5 (g, a, b, c, d, e, f);;
    let inv_curve = ICC.optimize_curve (ICC.inv curve)

    let primaries = {
      r = V2.v 0.64 0.33;
      g = V2.v 0.30 0.60;
      b = V2.v 0.15 0.06;
      w = Whitepoint.d65;
      f = curve
    }
    let space = `RGB primaries

    let linear_space = `RGB { primaries with f = `Linear }

    let flare =
      let amount = 0.0125 in
      `Gamma5 (1., 1. -. amount, 0., 1. -. amount, 0.0031308, amount, amount)
    let of_v3 v3 = `V3 v3, space
  end

  module YCbCr = struct
    type t = (sYCbCr,c3) sample
    let y (`V3 v, _) = V3.x v
    let cb (`V3 v, _) = V3.y v
    let cr (`V3 v, _) = V3.z v
  end
  module Yxy = struct
    type t = (sYxy,c3) sample
    let yy (`V3 v, _) = V3.x v
    let x (`V3 v, _) = V3.y v
    let y (`V3 v, _) = V3.z v
  end
  module Gray = struct
    type t = (sGray,c1) sample
    let gray (`V1 v, _) = v
    let linear_space = `Gray (Whitepoint.d65, `Linear)
  end
  module HSV = struct
    type t = (sHSV,c3) sample
    let h (`V3 v, _) = V3.x v
    let s (`V3 v, _) = V3.y v
    let v (`V3 v, _) = V3.z v
    let of_v3 v3 : t = `V3 v3, `HSV

    open SRGB
    let to_sRGB hsv = (* TODO eps comparisons ? *)
      let h = V3.x hsv in
      let s = V3.y hsv in
      let v = V3.z hsv in
      if s = 0. then of_v3 (V3.v v v v) else
      let sector = (h *. 360.) /. 60. in
      let i = floor sector in
      let f = sector -. i in
      let p = v *. (1. -. s) in
      let q = v *. (1. -. s *. f) in
      let t = v *. (1. -. s *. (1. -. f)) in
      let (v3:v3) = match truncate i with
      | 0 -> V3.v v t p
      | 1 -> V3.v q v p
      | 2 -> V3.v p v t
      | 3 -> V3.v p q v
      | 4 -> V3.v t p v
      | _ -> V3.v v p q in
      of_v3 v3;;

    let of_SRGB c = (* TODO eps comparisons ? *)
      let r = r c in
      let g = g c in
      let b = b c in
      let min =
        let m = if r < g then r else g in
        if m < b then m else b
      in
      let max =
        let m = if r > g then r else g in
        if m > b then m else b
      in
      if max = 0. then V3.v 0. 0. 0. else
      let delta = max -. min in
      let v = max in
      let s = delta /. max in
      let h =
        let h =
          if r == max then (g -. b) /. delta else
          if g == max then 2. +. (b -. r) /. delta else
          4. +. (r -. g) /. delta
        in
        let h = h *. 60. in
        if h < 0. then h +. 360. else h
      in
      V3.v h s v
  end
  module HLS = struct
    type t = (sHLS,c3) sample
    let h (`V3 v, _) = V3.x v
    let l (`V3 v, _) = V3.y v
    let s (`V3 v, _) = V3.z v
    let of_v3 v3 : t = `V3 v3, `HLS
  end
  module CMY = struct
    type t = (sCMY,c3) sample
    let c (`V3 v, _) = V3.x v
    let m (`V3 v, _) = V3.y v
    let y (`V3 v, _) = V3.z v
  end
  module CMYK = struct
    type t = (sCMYK,c4) sample
    let c (`V4 v, _) = V4.x v
    let m (`V4 v, _) = V4.y v
    let y (`V4 v, _) = V4.z v
    let k (`V4 v, _) = V4.w v
  end
  module CAM02Jab = struct
    type t = (sCAM02Jab,c3) sample
    let j (`V3 v, _) = V3.x v
    let a (`V3 v, _) = V3.y v
    let b (`V3 v, _) = V3.z v
  end
  module CAM02JCh = struct
    type t = (sCAM02JCh,c3) sample
    let j (`V3 v, _) = V3.x v
    let c (`V3 v, _) = V3.y v
    let h (`V3 v, _) = V3.z v
  end
  module IPT = struct
    type t = (sIPT,c3) sample
    let i (`V3 v, _) = V3.x v
    let p (`V3 v, _) = V3.y v
    let t (`V3 v, _) = V3.z v
  end
  module Colors = struct
    type t = (sColors,cn) sample
    let comp (`Vn v, _) i = v.(i)
    let v a = (`Vn a, `Colors (Array.length a))
  end

  let to_icc space = failwith "TODO"
  type conversion = float array -> float array
end

