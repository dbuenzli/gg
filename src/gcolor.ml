open Gg
open Raster
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
module Convert = struct
  type rendering_intent = [ `Perceptual | `Absolute_colorimetric |
                            `Relative_colorimetric | `Saturation ]
  type t = raster
  let to_srgb src intent = failwith "TODO"
  let to_lrgb src intent = failwith "TODO"
  let to_profile src intent dst scalar = failwith "TODO"
  let convert src intent = failwith "TODO"

  type reader1 = int -> color
  type reader2 = int -> int -> color
  type reader3 = int -> int -> int -> color
  let read1D raster ?(optimized=false) = failwith "TODO"
  let read2D raster ?(optimized=false) = failwith "TODO"
  let read3D raster ?(optimized=false) = failwith "TODO"
end
type whitepoint = [`Whitepoint of v3]
module Whitepoint = struct
  type t = whitepoint

  let v ~x ~y =
    `Whitepoint (V3.v (x /. y) 1. ((1. -. x -. y) /. y))

  let to_xy (`Whitepoint v) =
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
  let d50 = `Whitepoint (V3.v 0.9642 1.0 0.8249)
  (* CIE illuminant D50, according to ICC v4 specification and sRGB *)

  let d65 = `Whitepoint (V3.v 0.9504 1.0 1.0889)
  (** CIE illuminant D65.
   *
   * Everyone seems to define this with different truncations.
   *
   * x=0.31271 y=0.32902 => X=0.9504 Y=1.0 Z=1.0889
   * This is the definition used by http://www.color.org/chadtag.xalter
   * and sRGB_IEC61966-2-1*.icc
   * We use this definition.
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
  let e = `Whitepoint (V3.v 1. 1. 1.) (* CIE illuminant E (Equi-Power) *)
  let f8 = v ~x:0.34588 ~y:0.35875 (* CIE illuminant F8 *)
end

type space = [ Color.space | `CAM02Jab | `CAM02JCh | `LCh | `IPT ]
module ICC = struct
  (* TODO: could avoid polymorphic variants *)
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

  type curve = [`Identity | parametricCurve ]
  type 'a t = {
    model: [< space] as 'a;
    to_xyz: m3 option;
    chad: m3 option;
    curve1: curve;
    curve2: curve;
    curve3: curve;
    wp: whitepoint;(* have to use chad on this to get real wp *)
  }


  type ops = Curves of curve array | M3 of m3 (* TODO: nxm matrix, cLUT *)

  type processing_element = {
    inputs: int;
    op: ops;
  }

  type pipeline = processing_element list

  let m3_opt = function
    | Some m3 -> m3
    | None -> M3.id

  let build_toxyz icc =
    [ { inputs = 3; op = Curves [| icc.curve1; icc.curve2; icc.curve3 |]};
      { inputs = 3; op = M3 (m3_opt icc.to_xyz)};
      { inputs = 3; op = M3 (m3_opt icc.chad)};
    ]

  let minvalue = -65536.0

  let parameters_of_curve = function
    | `Identity ->
      1., 1., 0., 0., minvalue, 0., 0.
    | `Gamma1 g ->
      g, 1., 0., 0., minvalue, 0., 0.
    | `Gamma2 (g, a, b) ->
      g, a, b, 0., -. b /. a, 0., 0.
    | `Gamma3 (g, a, b, c) ->
      g, a, b, 0., -. b /. a, 0., c
    | `Gamma4 (g, a, b, c, d) ->
      g, a, b, c, d, 0., 0.
    | `Gamma5 (g, a, b, c, d, e, f) ->
      g, a, b, c, d, e, f

  (* Are the 2 numbers the same if they were encoded
   * into s15Fixed16Number? *)
  let same_sf32 a b =
    let n = (abs_float (a -. b)) < 1. /. 65536. in
    (*Printf.printf "comparing %.10g %.10g -> %b\n" a b n;*)
    n

  let build_curve (g, a, b, c, d, e, f) =
    if (same_sf32 a 1.) && (same_sf32 b 0.) && (same_sf32 e 0.) && same_sf32 d minvalue then
         if same_sf32 g 1. then `Identity
         else `Gamma1 g
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
    let d' = (a *. d +. b) ** g +. e in
    let e' = -. b /. a in
    let g' = 1. /. g in
    g', a', b', c', d', e', f'

  type pipe_run = float array -> int -> float array

  let compare_eps a b =
    let d = abs_float (a -. b) in
    if d < 1e-9 then 0
    else compare a b

  let is_mul_noop m1 m2 =
    M3.compare_f compare_eps (M3.mul m1 m2) M3.id

  let is_identity c1 c2 =
    let g1,a1,b1,c1,d1,e1,f1 = parameters_of_curve c1
    and g2,a2,b2,c2,d2,e2,f2 = inv c2 in
    same_sf32 g1 g2 && same_sf32 a1 a2 && same_sf32 b1 b2 && same_sf32 c1 c2 &&
    same_sf32 d1 d2 && same_sf32 e1 e2 && same_sf32 f1 f2

  let is_composition_identity ca1 ca2 =
    List.for_all2 is_identity (Array.to_list ca1) (Array.to_list ca2)

  let is_identity c =
    (build_curve (parameters_of_curve c)) = `Identity

  let are_identity_curves ca =
    List.for_all is_identity (Array.to_list ca)

  let optimize (inputs, accum) e =
    assert (inputs = e.inputs);
    let outputs = match e.op with
    | M3 _ -> 3
    | Curves ca -> Array.length ca in
    match e.op, accum with
    | M3 m2, {op = M3 m1;_} :: tl ->
        assert (e.inputs = 3);
        let m = M3.mul m1 m2 in
        if (M3.compare_f compare_eps m M3.id) = 0 then
          (* matrix is multipled with its inverse -> we can skip *)
          (outputs, tl)
        else
          (outputs, { inputs = 3; op = M3 m} :: tl)
    | M3 m, tl when (M3.compare_f compare_eps m M3.id) = 0 ->
      (* matrix is identity -> we can skip *)
      (outputs, tl)
    | Curves ca2, {op = Curves ca1;_} :: tl when is_composition_identity ca1 ca2 ->
        (outputs, tl)
    | Curves ca, tl when are_identity_curves ca ->
        (outputs, tl)
    | _ ->
        (* some other combination: not optimizable for now. *)
        (outputs, e :: accum)

  let optimize_pipeline inputs pipe =
    let _, rev1 = List.fold_left optimize (inputs, []) pipe in
    let _, pipe2 = List.fold_left optimize (inputs, []) (List.rev rev1) in
    pipe2
    (* TODO: check for 3D LUT optimization possibility *)

  let inv_op = function
    | {op=M3 m;_} -> {inputs=3;op=M3 (M3.inv m)}
    | {op=Curves ca;inputs} ->
        {inputs; op = Curves (Array.map (fun c -> build_curve (inv c)) ca)}

  let inv_pipeline pipe =
    List.rev_map inv_op pipe

  let build_ofxyz icc =
    inv_pipeline (build_toxyz icc)

  let link icc1 icc2 =
    optimize_pipeline 3 (*TODO*) ((build_toxyz icc1) @ (build_ofxyz icc2))

  let model icc = icc.model
  type primaries = {
    xr: float; yr: float;
    xg: float; yg: float;
    xb: float; yb: float
  }
  type v = V2 | V4
  let parse profile = failwith "TODO"
  let write ?v profile = failwith "TODO"

  let curve_identity = `Identity
  let curve_sRGB =
    let g = 2.4 and a = 1. /. 1.055 and b = 0.055 /. 1.055
    (* Y = (aX+b)^g wen X >= d*)
    and c = 1. /. 12.92 and d = 0.04045 in
    `Gamma4 (g, a, b, c, d)

  type viewing_conditions
  type cat = m3

  let chromatic_adapt ma ~src:(`Whitepoint src) ~dst:(`Whitepoint dst) =
    let sv = V3.ltr ma src and dv = V3.ltr ma dst in
    let m = M3.scale (V3.div dv sv) in
    M3.mul (M3.mul (M3.inv ma) m) ma;;

  let bradford = M3.v
      0.8951   0.2664  (-0.1614)
    (-0.7502)  1.7135    0.0367
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

  let whitepoint p =
    (* calculate real whitepoint, i.e. the one relating the the original
     * illuminant not the PCS (D50) illuminant *)
    match p.chad with
    | None -> p.wp
    | Some chad ->
      let `Whitepoint wp = p.wp in
      `Whitepoint (V3.ltr (M3.inv chad) wp)

  let chad p = p.chad
  let primaries p = failwith "TODO"
  let curve p = [| p.curve1; p.curve2; p.curve3 |]
  let viewing_conditions p = failwith "TODO"
  let to_xyz p = p.to_xyz

  let pXYZ ?(cat=bradford) wp = {
    model = `XYZ;
    to_xyz = None;
    chad = None;
    curve1 = `Identity;
    curve2 = `Identity;
    curve3 = `Identity;
    wp = wp
  }

  let xyz_of_xy x y = V3.v (x /. y) 1. ((1. -. x -. y) /. y)

  let pRGB ?(cat=bradford) primaries wp curve =
    let chad = chromatic_adapt cat ~src:wp ~dst:Whitepoint.d50 in
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
    let r = xyz_of_xy primaries.xr primaries.yr
    and g = xyz_of_xy primaries.xg primaries.yg
    and b = xyz_of_xy primaries.xb primaries.yb in
    let m1 = M3.of_cols r g b in
    let `Whitepoint w = wp in
    let y = V3.ltr (M3.inv m1) w in
    let m = M3.mul m1 (M3.scale y) in
    {
      model = `RGB;
      to_xyz = Some m;
      chad = Some chad;
      curve1 = curve;
      curve2 = curve;
      curve3 = curve;
      wp = Whitepoint.d50 (* because we use a chad tag! *)
    }

  let pLab ?cat wp = failwith "TODO"

  let pGray wp curve = failwith "TODO"

  let rec709 = {
    xr = 0.64; yr = 0.33;
    xg = 0.30; yg = 0.60;
    xb = 0.15; yb = 0.06;
  }
  let plRGB = pRGB rec709 Whitepoint.d65 curve_identity
  let psRGB = pRGB rec709 Whitepoint.d65 curve_sRGB

  let print_curve fmt curve = match (build_curve (parameters_of_curve curve))
  with
    | `Identity ->
        Format.fprintf fmt "id@\n"
    | `Gamma1 g ->
        Format.fprintf fmt "X^%.5f@\n" g
    | `Gamma2 (g,a,b) ->
        Format.fprintf fmt "(%.5f*X+%.5f)^%.5f when X >= -%.5f/%.5f@\n"
          a b g b a;
        Format.fprintf fmt "0 when X < -%.5f/%.5f@\n" b a
    | `Gamma3 (g,a,b,c) ->
        Format.fprintf fmt "(%.5f*X+%.5f)^%.5f when X >= -%.5f/%.5f@\n"
          a b g b a;
        Format.fprintf fmt "%.5f when X < -%.5f/%.5f@\n" c b a
    | `Gamma4 (g,a,b,c,d) ->
        Format.fprintf fmt "(%.5f*X+%.5f)^%.5f when X >= %.5f@\n"
          a b g d;
        Format.fprintf fmt "%.5f*X when X < %.5f@\n" c d
    | `Gamma5 (g,a,b,c,d,e,f) ->
        Format.fprintf fmt "(%.5f*X+%.5f)^%.5f+.%5f when X >= %.5f@\n"
          a b g e d;
        Format.fprintf fmt "%.5f*X+%.5f when X < %.5f@\n" c f d

  let print_pipeline fmt pipe =
    List.iter (fun e -> match e.op with
      | M3 m ->
          Format.fprintf fmt "Matrix:@[";
          M3.print fmt m;
          Format.fprintf fmt "@]@\n"
      | Curves ca ->
          Format.fprintf fmt "Curves:@[";
          Array.iter (print_curve fmt) ca;
          Format.fprintf fmt "@]@\n"
    ) pipe

  let () =
    print_pipeline Format.std_formatter (build_toxyz plRGB);
    let pipe = link psRGB psRGB in
    Format.fprintf Format.std_formatter "sRGB->sRGB: @\n";
    print_pipeline Format.std_formatter pipe;
    let pipe = link psRGB plRGB in
    Format.fprintf Format.std_formatter "sRGB->lRGB: @\n";
    print_pipeline Format.std_formatter pipe;
    let pipe = link plRGB psRGB in
    Format.fprintf Format.std_formatter "lRGB->sRGB: @\n";
    print_pipeline Format.std_formatter pipe;
    let pipe = link plRGB plRGB in
    Format.fprintf Format.std_formatter "lRGB->lRGB: @\n";
    print_pipeline Format.std_formatter pipe

  let pHSV () = failwith "TODO"
  let pHLS () = failwith "TODO"
  let pCAM02Jab vc = failwith "TODO"
  let pCAM02JCh vc = failwith "TODO"
  let pYCbCr () = failwith "TODO"
  let pCMY () = failwith "TODO"
  let pCMYK () = failwith "TODO"
  let pIPT () = failwith "TODO"
  let pGeneric n = failwith "TODO"

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


  let eval_type5 g a b c d e f x =
    if x >= d then
      (a *. x +. b) ** g +. e
    else
      c *. x +. f;;

  (* Are these 2 XYZ numbers the same after encoding
   * to XYZNumber? *)
  let same_xyz (`XYZ a) (`XYZ b) =
    V3.equal_f same_sf32 a b;;
end

type space1 = [ `Gray ]
type space3 = [ `XYZ | `Lab | `Luv | `YCbr | `Yxy | `RGB | `HSV | `HLS
              | `CMY | `CAM02Jab | `CAM02JCh | `LCh | `IPT ]
type space4 = [ `CMYK ]
type spacen = [ `CLR2 | `CLR3 | `CLR4 | `CLR5 | `CLR6 | `CLR7
              | `CLR8 | `CLR9 | `CLRA | `CLRB | `CLRC | `CLRD | `CLRE | `CLRF ]
module Sample = struct
  type 'a t = float array * 'a ICC.t

  let of_color color = [| V4.x color; V4.y color; V4.z color |], ICC.plRGB
  let convert sample p = failwith "TODO"

  let to_color sample = convert sample ICC.plRGB

  let model (_,icc) = ICC.model icc

  let v1 p c1 = [| c1 |], p
  let v3 p c1 c2 c3 = [| c1; c2; c3 |], p
  let v4 p c1 c2 c3 c4 = [| c1; c2; c3; c4 |], p
  let vn p cn = cn, p

  type 'a sample1 = ([< space1 ] as 'a) t
  type 'a sample3 = ([< space3 ] as 'a) t
  type 'a sample4 = ([< space4 ] as 'a) t
  type 'a samplen = ([< spacen ] as 'a) t

  let gray (v, {ICC.model = `Gray;_}) = v.(0)
  let i (v, {ICC.model = `IPT;_}) = v.(0)
  let j (v, {ICC.model = `CAM02Jab | `CAM02JCh;_}) = v.(0)
  let r (v, {ICC.model = `RGB;_}) = v.(0)
  let y' (v,{ICC.model = `YCbr|`Yxy;_}) = v.(0)

  let a (v, {ICC.model = `Lab | `CAM02Jab;_}) = v.(1)
  let cb (v, {ICC.model = `YCbr;_}) = v.(1)
  let g (v, {ICC.model = `RGB;_}) = v.(1)
  let p (v, {ICC.model=`IPT;_}) = v.(1)
  let u (v, {ICC.model=`Luv;_}) = v.(1)

  let b (v, {ICC.model = `Lab | `RGB | `CAM02Jab;_}) = v.(2)
  let cr (v, {ICC.model = `YCbr;_}) = v.(2)
  let t (v, {ICC.model = `IPT;_}) = v.(2)
  let v (v, {ICC.model=`Luv|`HSV;_}) = v.(2)
  let z (v, {ICC.model=`XYZ;_}) = v.(2)

  let c (v, icc) = match icc.ICC.model with
  | `LCh | `CAM02JCh -> v.(1)
  | `CMY | `CMYK -> v.(0)

  let h (v, icc) = match icc.ICC.model with
  | `LCh | `CAM02JCh -> v.(2)
  | `HSV | `HLS -> v.(0)

  let l (v, icc) = match icc.ICC.model with
  | `Lab | `LCh | `Luv -> v.(0)
  | `HLS -> v.(1)

  let s (v, icc) =  match icc.ICC.model with
  | `HSV -> v.(1)
  | `HLS -> v.(2)

  let x (v, icc) = match icc.ICC.model with
  | `XYZ -> v.(0)
  | `Yxy -> v.(1)

  let y (v, icc) = match icc.ICC.model with
  | `XYZ -> v.(1)
  | `Yxy -> v.(2)

  let comp (sample,_) i = sample.(i)
  let dim (sample,_) = Array.length sample

  let deltaE (lab1,p1) (lab2,p2) =
    if (ICC.whitepoint p1) <> (ICC.whitepoint p2) then
      invalid_arg "deltaE: can only compare Lab values with same whitepoint";
    let dL = lab1.(0) -. lab2.(0)
    and da = lab1.(1) -. lab2.(1)
    and db = lab1.(2) -. lab2.(2) in
    sqrt (dL *. dL +. da *. da +. db *. db)

  let lab_c lab = sqrt (lab.(1) *. lab.(1) +. lab.(2) *. lab.(2))

  let cie94_deltaE (lab1,p1) (lab2,p2) =
    if (ICC.whitepoint p1) <> (ICC.whitepoint p2) then
      invalid_arg "deltaE: can only compare Lab values with same whitepoint";
    let dL = lab1.(0) -. lab2.(0)
    and c1 = lab_c lab1 and c2 = lab_c lab2 in
    let dC = c1 -. c2
    and da = lab1.(1) -. lab2.(1)
    and db = lab1.(2) -. lab2.(2) in
    let dH2 = da *. da +. db *. db -. dC *. dC
    and sC = 1. +. 0.048 *. c1
    and sH = 1. +. 0.014 *. c2 in
    sqrt (dL *. dL +. (dC *. dC /. (sC *. sC)) +. (dH2 /. (sH *. sH)));;
end
