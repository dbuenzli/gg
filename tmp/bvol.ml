module Inter = struct
  type test =  [ `Excl  | `Incl | `Inter ]  
  type etest = [ `Excl | `Incl | `Inter ]
end


module type S = sig
  type t
  val bound_pts : 'a Pts3.t -> t
  val bound_vols : t list -> t
  val itest : t -> t -> Inter.test
  val itest_frust : Frust3.t -> t -> Inter.test
  val itest_pt : v3 -> t -> Inter.test
end
