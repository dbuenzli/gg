(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)
type t = V4.t
let make ~n ~d = [| n.(0); n.(1); n.(2); -. d |]
let of_pts ~o u v = 
  let n = V3.normalize (V3.cross (V3.sub u o) (V3.sub v o)) in
  let d = -. (V3.dot n o) in
  make n d
   
let of_n_pt ~n p = make n (-. (V3.dot n p)) 
let n p = p
let d p = -. p.(3)
let dist p pt = (V3.dot p pt) +. p.(3)
let proj p pt = V3.sub pt (V3.smul (dist p pt) p)
let sym p pt = V3.sub pt (V3.smul (2. *. (dist p pt)) p)
let compare = Pervasives.compare
let to_v4 x = x
let of_v4 x = x 

let print f p = 
  Format.fprintf f 
    "@[<hov 1>(n =@ %a; d =@ %F)@]" V3.print p (-. p.(3))

let to_string p = 
  Format.fprintf Format.str_formatter "%a" print p; 
  Format.flush_str_formatter ()
