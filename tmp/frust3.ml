(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

type t = 
    { left : Plane3.t;
      right : Plane3.t;
      bottom : Plane3.t;
      top : Plane3.t;
      near : Plane3.t;
      far : Plane3.t }

let create ~left ~right ~bottom ~top ~near ~far = 
  { left = left;
    right = right;
    bottom = bottom;
    top = top;
    near = near;
    far = far }


let left f = f.left
let right f = f.right
let bottom f = f.bottom
let top f = f.top
let near f = f.near
let far f = f.far
let planes f = [ f.left; f.right; f.bottom; f.top; f.near; f.far]
let of_m4 m =
  let p = V4.make in
  { left = p (-. (m.(3) +. m.(0))) (-. (m.(7) +. m.(4))) 
      (-. (m.(11) +. m.(8))) (-. (m.(15) +. m.(12)));
    right = p (-. (m.(3) -. m.(0))) (-. (m.(7) -. m.(4))) 
      (-. (m.(11) -. m.(8))) (-. (m.(15) -. m.(12)));
    bottom = p (-. (m.(3) +. m.(1))) (-. (m.(7) +. m.(5))) 
      (-. (m.(11) +. m.(9))) (-. (m.(15) +. m.(13)));
    top = p (-. (m.(3) -. m.(1))) (-. (m.(7) -. m.(5))) 
      (-. (m.(11) -. m.(9))) (-. (m.(15) -. m.(13)));
    near = p (-. (m.(3) +. m.(2))) (-. (m.(7) +. m.(6))) 
      (-. (m.(11) +. m.(10))) (-. (m.(15) +. m.(14)));
    far = p (-. (m.(3) -. m.(2))) (-. (m.(7) -. m.(6))) 
      (-. (m.(11) -. m.(10))) (-. (m.(15) -. m.(14))); }
  
let print fmt f = 
  let pp = Plane3.print in
  Format.fprintf fmt 
    "@[<hov 1>(left =@ %a;@ right =@ %a;@ bottom =@ %a;@ top =@ %a;@ \
               near =@ %a;@ far =@ %a;@ )@]"
  pp f.left pp f.right pp f.bottom pp f.top pp f.near pp f.far

let to_string f =
  Format.fprintf Format.str_formatter "%a" print f; 
  Format.flush_str_formatter ()
      
