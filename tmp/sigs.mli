(** {3 Point soups} 

    Points soups allow to read and write sets of points in an implementation
    independent way. *)

module Pts3 : sig

  type 'a acc = 
      { count : 'a -> int; (** Number of points. *)
	el : 'a -> int -> v3; (** Point reader. *)
	set_el : 'a -> int -> v3 -> unit (** Point writer. *)}	
  (** Accessor for point soups with implementation 'a. *)

  type access = [ `R | `W ]
  (** Point soup access policy, read or read and write. *)

  type +'a t constraint 'a = [< access ]  
  (** The type for generic point soups. *)

  val make : 'a acc -> ([< access] as 'b) -> 'a -> 'b t
  (** Creates a point soup with the given access policy and implementation. *)
  val count : 'a t -> int
  (** Number of elements in the point soup. *)
  val el : 'a t -> int -> v3
  (** Element of a point soup. *)
  val set_el : [`W] t -> int -> v3 -> unit
  (** Sets the element of a point soup. *)
  val iter : (v3 -> unit) -> 'a t -> unit
  (** Iterates over the soup's elements. *)
  val iteri : (int -> v3 -> unit) -> 'a t -> unit
  (** Same as {!Gmath.Pts3.iter} but the function also gets the index. *)
  val set_map : (v3 -> v3) -> 'a t -> [`W] t -> unit
  (** Maps elements of a point soup to another one. *)
  val set_mapi : (int -> v3 -> v3) -> 'a t  -> [`W] t -> unit
  (** Same as {!Gmath.Pts3.set_map} but the function also gets the index. *)
  val fold_left : ('a -> v3 -> 'a) -> 'a -> 'b t -> 'a
  (** Left folds the soup's elements. *)
  val fold_right : (v3 -> 'a -> 'a) -> 'b t -> 'a -> 'a
  (** Right folds the soup's elements. *)
  val iter2 : (v3 -> v3 -> unit) -> 'a t -> 'b t -> unit
  (** Iterates over the two soup's elements. *)
  val set_map2 : (v3 -> v3 -> v3) -> 'a t -> 'b t -> [`W] t -> unit
  (** Maps elements of the two point soups to another one. *)
  val fold_left2 : ('a -> v3 -> v3 -> 'a) -> 'a -> 'b t -> 'c t -> 'a
  (** Left folds the two soup's elements. *)
  val fold_right2 : (v3 -> v3 -> 'a -> 'a) -> 'b t -> 'c t -> 'a -> 'a
  (** Right folds the two soup's elements. *)
end

(** {3 Lines} *)

module Line2 : sig
  type t 
  (** The type for lines. *)
  val make : o:v2 -> dir:v2 -> t
  (** Line with origin [o] and direction [dir]. *)
  val of_pts : v2 -> v2 -> t
  (** Line with origin [a] and normalized direction [ab]. *)
  val o : t -> v2
  (** Line origin. *)
  val dir : t -> v2
  (** Line direction. *)
  val pt : t -> at:float -> v2
  (** Returns the point [o + t * dir]. *) 
  val dist : t -> v2 -> float
  (** Distance between the point and the line. *)
  val param : t -> v2 -> float
  (** Returns [t] such that [p' = o + t * dir] where p' is the projection of 
      the point on the line. *)
  val proj : t -> v2 -> v2
  (** Projection of the point on the line. *)
  val sym : t -> v2 -> v2
  (** Symmetric of the point across the line. *)
  val compare : t -> t -> int
  (** Same as [Pervasives.compare] (i.e. lexicographic comparison). *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
end

module Line3 : sig
  type t 
  (** The type for lines. *)
  val make : o:v3 -> dir:v3 -> t
  (** Line with origin [o] and direction [dir] (must be normalized). *)
  val of_pts : v3 -> v3 -> t
  (** Line with origin [a] and normalized direction [ab]. *)
  val o : t -> v3
  (** Line origin. *)
  val dir : t -> v3
  (** Line direction. *)
  val pt : t -> at:float -> v3
  (** Returns the point [o + t * dir]. *) 
  val dist : t -> v3 -> float
  (** Distance between the point and the line. *)
  val param : t -> v3 -> float
  (** Returns [t] such that [p' = o + t * dir] where p' is the projection of 
      the point on the line. *)
  val proj : t -> v3 -> v3
  (** Projection of the point on the line. *)
  val sym : t -> v3 -> v3
  (** Symmetric of the point across the line. *)
  val compare : t -> t -> int
  (** Same as [Pervasives.compare] (i.e. lexicographic comparison). *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
end

(** {3 Planes} *)

module Plane3 : sig
  type t
  (** The type for planes. *)
  val make : n:v3 -> d:float -> t
  (** Plane at distance [d] from the origin along the normal [n]
      (must be normalized). *) 
  val of_pts : o:v3 -> v3 -> v3 -> t
  (** Plane defined by three non-collinear points. The normal is given
  by normalizing [(a - o) x (b - o)]*) 
  val of_n_pt : n:v3 -> v3 -> t
  (** Plane defined by the normal [n] (must be normalized) at the
  given point. *) 
  val n : t -> v3
  (** Plane normal. *)
  val d : t -> float 
  (** Distance from the plane to the origin along the normal's direction. *)
  val dist : t -> v3 -> float
  (** Signed distance between the point and the plane. *)
  val proj : t -> v3 -> v3 
  (** Projection of the point on the plane. *)
  val sym : t -> v3 -> v3
  (** Symmetric of the point across the plane. *)
  val compare : t -> t -> int
  (** Same as [Pervasives.compare] (i.e. lexicographic comparison). *)
  val to_v4 : t -> v4
  (** The plane's parameters in a vector. First three components are 
      the normal, last component is the {e negated} distance to the
      origin along the normal *)
  val of_v4 : v4 -> t
  (** See {!Gmath.Plane3.to_v4} *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
end

(** {3 Bounding volumes and surfaces} *)

module Frust3 : sig

(** 3D frustum.

    A frustum is a truncated quadrilateral (usually rectangular) pyramid. *)
  
  type t 
  (** The type for frusta. *)

  val create : left:Plane3.t -> right:Plane3.t -> bottom:Plane3.t -> 
    top:Plane3.t -> near:Plane3.t -> far:Plane3.t -> t
  (** Creates a frustum with the given bounding planes. *)

  val left : t -> Plane3.t
  val right : t -> Plane3.t
  val bottom : t -> Plane3.t
  val top : t -> Plane3.t
  val near : t -> Plane3.t
  val far : t -> Plane3.t
  val planes : t -> Plane3.t list
  (** Returns the planes in the order left, right, bottom, top, near, far. *)

  val of_m4 : m4 -> t
  (** Extracts a frustum from the matrix. *)

  val to_string : t -> string
  val print : Format.formatter -> t -> unit
end

(** Types for intersection results. *)
module Inter : sig
  type test =  [ `Excl  | `Incl | `Inter ]  
  (** Intersection test result. The algorithm may return [`Inter] even though
      the second space bound is [`Incl] in the first. *)
  
  type etest = [ `Excl | `Incl | `Inter ]
  (** Exact intersection test result. The algorithm may not return [`Inter]
      if the second space bound [`Incl] in the first. *)
end

(** Implemented by all bounding volumes. *)
module type Bvol = sig
  type t
  (** The type for bounding volumes. *)

  val bound_pts : 'a Pts3.t -> t
  (** Volume bounding the point soup. *)

  val bound_vols : t list -> t
  (** Volume bounding volume list. *)
 
  val itest : t -> t -> Inter.test
  (** Intersection between two bounding volumes. *)

  val itest_frust : Frust3.t -> t -> Inter.test
  (** Intersection between a frustum and a bounding volume. *)

  val itest_pt : v3 -> t -> Inter.test
  (** Intersection between a point and a bounding volume. *)
end

module Sphere3 : sig
  type t 
  (** The type for spheres. *)
  val make : c:v3 -> r:float -> t
  (** Sphere with center [c] and radius [r]. *) 
  val of_pts : v3 -> v3 -> t
  (** Shpere with center [a] going through point [b]. *)
  val c : t -> v3
  (** Sphere center. *)
  val r : t -> float
  (** Sphere radius. *)
  val bound_pts : 'a Pts3.t -> t
  val bound_vols : t list -> t
  val itest : t -> t -> Inter.test
  val itest_frust : Frust3.t -> t -> Inter.test
  val itest_pt : v3 -> t -> Inter.test
  val compare : t -> t -> int
  (** Same as [Pervasives.compare] (i.e. lexicographic comparison). *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
end



(** {3 Miscellaneous} *)

module Noise : sig
  (** Noise fields. *)

  (** {3 Perlin's improved noise} 

      Ken Perlin, {e Improving noise}, proceedings of SIGGRAPH, 2002
      \[{{:http://doi.acm.org/10.1145/566570.566636}doi}\]. *)


  (** {3 Perlin's simplex noise} 
      
      Stefan Gustavson, 
      {{:http://www.itn.liu.se/~stegu/simplexnoise}
      {e Simplex noise demystified}}, 2005. *)

  
  val simplex1 : float -> float 
  val simplex2 : float -> float -> float 
  val simplex3 : float -> float -> float -> float
  val simplex4 : float -> float -> float -> float -> float

  val turbulence3 : (* (float -> float -> float -> float) -> *)
      float -> float -> float -> float -> float 
  val turbulence2 : (* (float -> float -> float -> float) -> *)
      float -> float -> float -> float 
  (** {3 Olano's modified noise} 

      Marc Olano, {e Modified noise for Evaluation on Graphics Hardware},
      proceedings of Workshop on Graphics Hardware, 2005.
      \[{{:http://doi.acm.org/10.1145/1071866.1071883}doi}\]. *)

(*
  val olano1 : float -> float
  val olano2 : float -> float -> float
  val olano3 : float -> float -> float -> float
  val olano4 : float -> float -> float -> float -> float


  module V : sig
    
   (** Vectorial interface *)

    val simplex1 : float -> float
    val simplex2 : v2 -> float
    val simplex3 : v3 -> float
    val simplex4 : v4 -> float

    val olano1 : float -> float
    val olano2 : v2 -> float
    val olano3 : v3 -> float
    val olano4 : v4 -> float	
  end
*)
end
