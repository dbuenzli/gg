(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)
type t = V2.t * V2.t

let make ~o ~dir = (o,dir)
let of_pts p p' = (p, V2.sub p' p)
let o (o,d) = o
let dir (o,d) = d
let pt (o,d) ~at = V2.add o (V2.smul at d)
let dist (o,d) pt = abs_float (V2.dot (V2.sub pt o) (V2.ortho d)) 
let param (o,d) pt = V2.dot (V2.sub pt o) d 
let proj ((o,d) as l) pt = V2.add o (V2.smul (param l pt) d)
let sym ((o,d) as l) pt = 
  V2.add (V2.sub (V2.smul 2. o) pt) (V2.smul (2. *. (param l pt)) d)

let compare = Pervasives.compare
let print fmt (o,d) = 
  Format.fprintf fmt "@[<hov 1>(o =@ %a; dir =@ %a)@]" V2.print o V2.print d

let to_string l = 
  Format.fprintf Format.str_formatter "%a" print l; 
  Format.flush_str_formatter ()


