(* Very early draft of Bigarray, Gg.Color, Gg.Raster and ICC profile integration *)
open Gg

module Convert : sig
  type rendering_intent = [ `Perceptual | `Absolute_colorimetric |
                            `Relative_colorimetric | `Saturation ]
  type t = raster
  val to_srgb : t -> rendering_intent -> t
  (** [to_srgb intent source] is a function that creates a new raster
    with samples in 8-bit per component sRGB format, similar to
    [convert_to_profile]

    You can use this function if you want to save an image with a standard sRGB color
    profile, or if you want to load an OpenGL texture with EXT_texture_sRGB.
   *)

  val to_lrgb : t -> rendering_intent -> t
  (** [convert_to_lrgb source] is a function that creates a new raster
    with samples in 16-bit per component linearized sRGB format, similar to
    [convert_to_profile]

    You can use this function to load an image and perform linear operations on
    its pixels, or to load an OpenGL texture without EXT_texture_sRGB.
   *)

  val to_profile : t -> rendering_intent -> Color.profile -> Raster.scalar_type -> t
  (** [convert_to_profile source intent profile scalar] is a function that
      creates a new raster with the specified [profile] and [scalar_type],
      and same dimensions as the original raster.
      The samples will have an alpha component if the source had an alpha component
   
      The conversion is a no-op if the [source] is already in the specified format:
      in this case the source raster is returned as is.

      See [convert] if you would like to store the converted colors
      in an already existing raster, or if you want more control over the 
      raster's format.
   *)

  val convert: t -> rendering_intent -> t -> unit
  (** [convert source intent destination] is a function that converts color samples
    from the source Raster to the destination Raster.
    The conversion is performed is performed according to the ICC profile of each
    rasters.
    It is NOT allowed for the two rasters to share the same buffer
   
    Raises: Invalid_argument if raster sizes do not match, or if
    ICC profile is missing, and the sample format has no default.
   *)

  type reader1 = int -> color
  type reader2 = int -> int -> color
  type reader3 = int -> int -> int -> color

  val read1D : raster -> ?optimized:bool -> reader1
  (** [read1d raster] is a function to start reading Gg.color samples from a
      raster (and convert to the linear sRGB colorspace of Gg.color).
  
      [optimized] defaults to false.
      Set this to true to convert the raster to a temporary raster with a linear
      sRGB color profile.
      This will speed up [reader1] invocations at the expense of more memory
      needed.
  
      Raises: Invalid_argument if Gg.raster is not a 1D raster
   *)

  val read2D : raster -> ?optimized:bool -> reader2
  (** [read2d raster] is a function to start reading Gg.color samples from a
      raster, like [read1D] but for 2D rasters. *)
 
  val read3D : raster -> ?optimized:bool -> reader2
  (** [read3d raster] is a function to start reading Gg.color samples from a
      raster, like [read1D] but for 3D rasters. *)
end

(* Colorspaces *)

type whitepoint = [`Whitepoint of v3]
module Whitepoint : sig
  type t = whitepoint
  val v : x:float -> y:float -> t
  (** [v x y] is a function that creates a whitepoint based
      on the specified xy chromacities. *)

  val of_temperature: float -> t
  (** [of_temperature cct] is a function that calculates the
      whitepoint of a daylight illuminant with specified correlated color
      temperature. *)

  val to_xy : t -> v2

  val d50 : t
  (** CIE illuminant D50 with X=0.9642 Y=1.0 Z=0.8249 *)

  val d55 : t
  (** CIE illuminant D55 *)

  val d65 : t
  (** CIE illuminant D65 *)

  val d93 : t
  (** D93 illuminant, CCT of 9305 K *)

  val a : t
  (** CIE illuminant A *)

  val e : t
  (** CIE illuminant E (Equi-Power) *)

  val f2 : t
  (** CIE illuminant F2 *)

  val f8 : t
  (** CIE illuminant F8 *)
end

type space1 = [ `Gray ]
type space3 = [ `XYZ | `Lab | `Luv | `YCbr | `Yxy | `RGB | `HSV | `HLS
              | `CMY | `CAM02Jab | `CAM02JCh | `LCh | `IPT ]
type space4 = [ `CMYK ]
type spacen = [ `CLR2 | `CLR3 | `CLR4 | `CLR5 | `CLR6 | `CLR7
              | `CLR8 | `CLR9 | `CLRA | `CLRB | `CLRC | `CLRD | `CLRE | `CLRF ]
type space = [ space1 | space3 | space4 | spacen ]
module ICC : sig
  type 'a t constraint 'a = [< space ]
  val model : 'a t -> 'a
  type primaries = {
    xr: float; yr: float;
    xg: float; yg: float;
    xb: float; yb: float
  }
  type v = V2 | V4
  val parse : Color.profile -> space t
  (** [parse icc] is a function that parses the binary ICC profile [icc].
   * ICC profiles v2 and v4 are supported.
   * *)

  val write : ?v:v -> 'a t -> Color.profile
  (** [write profile] is a function that generates the binary ICC profile 
   * corresponding to [profile].
   * [v] defaults to the version of [profile].
   * If specified it will convert the profile to the specified version.
   * Note: converting from v4 to v2 can loose precision.
   *)

  (* TODO: curves, etc. *)

  type curve
  type viewing_conditions
  type cat = m3

  val curve_identity: curve

  (* Profile parameters *)
  val bradford: cat
  (** The Bradford chromatic adaptation matrix.
   * This is the one used by ICC profiles usually, and the recommended one to
   * use when creating ICC profiles.
   * It is not the best matrix though *)

  val cat02: cat
  (** CAT02 matrix as used by CIECAM02.
   
    This should be better than Bradford, however it can produce
    negative XYZ values for certain colors.
    There are 2 unofficial corrected versions that don't produce negative
    colors, but slightly worse results overall.
    We are using the original matrix until CIE TC8-11 adopts another.
   *)

  val whitepoint : 'a t -> whitepoint
  val chad : 'a t -> m3 option
  (** The chromatic adaptation transform that transforms an XYZ color
   * from the source whitepoint to the D50 whitepoint.
   * It can be constructed by using XYZ.chromatic_adapt. *)
  val primaries : [< `RGB | `HSV | `HLS] t -> primaries
  val curve : 'a t -> curve array
  val viewing_conditions : 'a t -> viewing_conditions
  val to_xyz : 'a t -> m3 option
  (** If the transformation is linear then return a matrix that transforms
   * color samples from the profile's colorspace to XYZ.
   * If the transformation is not (known to be) linear then None is returned.
   * *)

  val pXYZ : ?cat:cat -> whitepoint -> [`XYZ] t
  val pRGB : ?cat:cat -> primaries -> whitepoint -> curve -> [`RGB] t
  val pLab : ?cat:cat -> whitepoint -> [`Lab] t
  val plRGB : [`RGB] t (* the Colorspace of Gg.color *)
  val psRGB:  [`RGB] t (* the Colorspace of Gg.srgba *)
  val pGray: whitepoint -> curve -> [`Gray] t
  val pHSV : unit -> [`HSV] t
  val pHLS : unit -> [`HLS] t
  val pCAM02Jab : viewing_conditions -> [`CAM02Jab] t
  val pCAM02JCh : viewing_conditions -> [`CAM02JCh] t
  val pYCbCr : unit -> [`YCbr] t (* TODO *)
  val pCMY : unit -> [`CMY] t (* TODO *)
  val pCMYK : unit -> [`CMYK] t (* TODO *)
  val pIPT : unit -> [`IPT] t (* TODO *)
  val pGeneric : int -> spacen t (* TODO *)
end

module Sample : sig
  type 'a t constraint 'a = [< space ]

  type 'a sample1 = ([< space1 ] as 'a) t
  type 'a sample3 = ([< space3 ] as 'a) t
  type 'a sample4 = ([< space4 ] as 'a) t
  type 'a samplen = ([< spacen ] as 'a) t

  val of_color : color -> [`RGB] t
  val to_color : 'a t -> color
  val model : 'a t -> 'a
  (** The model of this color sample *)

  val v1: 'a ICC.t -> float -> 'a sample1
  val v3: 'a ICC.t -> float -> float -> float -> 'a sample3
  val v4: 'a ICC.t -> float -> float -> float -> float -> 'a sample4
  val vn: 'a ICC.t -> float array -> 'a samplen

  val convert : 'a t -> 'b ICC.t -> 'b t
  (** [convert sample profile]
   * Convert a color sample from any colorspace to the current colorspace with
   * [dst] profile (or a default profile if not specified),
   * adapting to the destination's whitepoint if necessary. *)

  val a : [< `Lab | `CAM02Jab ] t -> float
  val b : [< `Lab| `RGB | `CAM02Jab ] t -> float
  val cb: [< `YCbr] t -> float
  val c : [< `LCh | `CMY | `CMYK | `CAM02JCh] t -> float
  val cr: [< `YCbr] t -> float
  val gray: [< `Gray] t -> float
  val g : [< `RGB] t -> float
  val h : [< `LCh | `HSV | `HLS | `CAM02JCh ] t -> float
  val i : [< `IPT] t -> float
  val j : [< `CAM02Jab | `CAM02JCh] t -> float
  val l : [< `Lab | `LCh | `Luv | `HLS] t -> float
  val p : [< `IPT] t -> float
  val r : [< `RGB] t -> float
  val s : [< `HSV | `HLS] t -> float
  val t : [< `IPT] t -> float
  val u : [< `Luv] t -> float
  val v : [< `Luv| `HSV ] t -> float
  val x : [< `XYZ | `Yxy] t -> float
  val y : [< `XYZ | `Yxy] t -> float
  val y': [< `YCbr | `Yxy] t -> float
  val z : [< `XYZ] t -> float
  val comp: 'a t -> int -> float
  val dim: 'a t -> int

  val deltaE : [`Lab] t -> [`Lab] t -> float
  val cie94_deltaE : [`Lab] t -> [`Lab] t -> float
end
