
(** Delaunay triangulation *)

module type Pt = 
    type t 
end

module Make (Pt : Pt) = struct
  type t
  (** The type for triangulations *)

  val empty : t

  val add_pt : t -> Pt.t -> t

  val iter : (Pt.t -> Pt.t -> unit) -> t -> 'a 
  val fold : (Pt.t -> Pt.t -> 'a -> 'a) -> t -> 'a -> 'a
end


