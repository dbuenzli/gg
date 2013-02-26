(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

open Gg;;

module type S = sig 
  (** 
      Raster image meta-data.

      This module defines a type to describe the data layout of 2D and 3D
      raster images. The actual type to hold the image data is not
      specified by this module and the pixel format specification is
      color space agnostic.

      A raster image is a rectangular volume of pixels. Pixels are
      made of a certain number of {e components}. Each component has
      the same number of bits, 8, 16 or 32 and the same
      representation, either floating point or integer.  

      The first pixel is the image's bottom, left, frontmost pixel. *)

 type alpha = [ `After | `Before ]
 (** Alpha component location. For images with an alpha component, 
     specifies if it is stored before or after the color components. *)
       
 type csize = [ `C8 | `C16 | `C32 ]
 (** Number of bits per components. *)
	
 val csize : csize -> int

 type crep = [ `Float | `Uint ]
 (** Component representation. *)

 type pf = {
  comps : int; (** Number of components. *)
  csize : csize; (** Number of bits per components. *)
  crep : crep; (** Component representation. *)
  alpha : alpha option; (** Alpha component location (if any). *) }
 (** The type for pixel formats. *)

 type t
 (** The type for raster image meta-data. *)

 val make : ?first:float -> ?skip:V2.t -> extent:V3.t -> pf -> t
 (** Image meta data.
     {ul 
     {- [first], number of pixels to skip to find the bottom, left,
        frontmost pixel [(0,0,0)].}
     {- [skip], on [x] number of {e pixels} to skip 
     between two consecutive lines, defaults to [0]. On [y], number
     of {e lines} to skip between two consecutive images (for 3D), 
     defaults to [0].}
     {- [extent], image width, height and depth. The minimal value is [1] in
        each dimension.}}*)

 val extent : t -> V3.t
 val extent2 : t -> V2.t
 (** Width and height. *)
 val pf : t -> pf
 val skip : t -> V2.t 
 val first : t -> float

 val sub : ?o:V3.t -> ?extent:V3.t -> t -> t
 (** Subimage from the given image. 
     {ul
     {- [o], new origin (bottom, left, frontmost pixel) of the image 
     in pixels, defaults to {!V3.o}.}
     {- [extent], new extent, defaults to the original size minus the 
     new origin.}} *)
end
      
type alpha = [ `After | `Before ]
type csize = [ `C8 | `C16 | `C32 ]
let csize = function `C8 -> 8 | `C16 -> 16 | `C32 -> 32

type crep = [ `Float | `Uint ]
type pf = 
    { comps : int; 
      csize : csize; 
      crep : crep; 
      alpha : alpha option;}
      
type t = 
    { first : float;
      pitch : V2.t;
      extent : V3.t;
      pf : pf; }

let invalid s s' = invalid_arg (Printf.sprintf s s')

let make ?(first = 0.) ?(skip = V2.o) ~extent pf = 
  let skip_w = V2.x skip in
  let skip_h = V2.y skip in
  let w = V3.x extent in
  let h = V3.y extent in 
  let d = V3.z extent in 
  if (w < 1. || h < 1. || d < 1.) then  
    invalid "invalid extent %s" (V3.to_string extent)
  else if (skip_w < 0. || skip_h < 0.) then
    invalid "invalid skip %s" (V2.to_string skip)
  else
    { first = first;
      pitch = V2.add (V3.to_v2 extent) skip;
      extent = extent;
      pf = pf }
      
let extent i = i.extent
let extent2 i = V3.to_v2 i.extent
let pf i = i.pf
let skip i = V2.sub i.pitch (V3.to_v2 i.extent)
let first i = i.first
let sub ?(o = V3.o) ?extent i = failwith "unipml"


      
    
