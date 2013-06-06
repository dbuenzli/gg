open Gg
type whitepoint = Whitepoint of v3
module Whitepoint : sig
  type t = whitepoint
  val v : x:float -> y:float -> t
  (** [v x y] is a function that creates a whitepoint based
      on the specified xy chromacities. *)

  val of_temperature: float -> t
  (** [of_temperature cct] is a function that calculates the
      whitepoint of a daylight illuminant with specified correlated color
      temperature. *)

  val d50 : t
  (** CIE illuminant D50 with X=0.9642 Y=1.0 Z=0.8249 *)

  val d55 : t
  (** CIE illuminant D55 *)

  val d65 : t
  (** CIE illuminant D65 (X=0.9504 Y=1.0 Z=1.0889) *)

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

module XYZ : sig
  type t = v3
  val of_xy : float -> float -> t

  type cat = CAT of m3
  (** A chromatic adaptation transformation *)

  val bradford : cat
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

  val chromatic_adapt : cat -> src:whitepoint -> dst:whitepoint -> m3
end

module RGB : sig
  type primaries = {
    xr: float; yr: float;
    xg: float; yg: float;
    xb: float; yb: float
  }
  val rec709 : primaries
  (** The Rec 709 primaries (sRGB, HDTV) *)

  val to_xyz : primaries -> whitepoint -> m3
  (** Matrix to transform linear RGB values to XYZ (same whitepoint as RGB) *)

  val to_pcsxyz : ?cat:XYZ.cat -> primaries -> whitepoint -> m3
  (** Matrix to transform linear RGB values to XYZ D50 *)

  val srgb_to_xyz_d65 : m3
  (** Matrix to transform linear sRGB values to XYZ D65 *)

  val srgb_to_pcsxyz : m3
  (** Matrix to transform linear sRGB values to PCS XYZ (D50) *)
end
