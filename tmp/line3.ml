(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)
type t = { o : V3.t; dir : V3.t }

let make ~o ~dir = { o = o; dir = V3.normalize dir }
let of_pts p p' = { o = p; dir =  V3.sub p' p }
let o l = l.o
let dir l = l.dir
let pt l ~at = V3.add l.o (V3.smul at l.dir)
let dist l pt = V3.norm (V3.cross (V3.sub pt l.o) l.dir)
let param l pt = V3.dot (V3.sub pt l.o) l.dir 
let proj l pt = V3.add l.o (V3.smul (param l pt) l.dir)
let sym l pt = 
  V3.add (V3.sub (V3.smul 2. l.o) pt) (V3.smul (2. *. (param l pt)) l.dir)

let compare = Pervasives.compare
let print fmt l = 
  Format.fprintf fmt "@[<hov 1>(o =@ %a; dir =@ %a)@]" 
    V3.print l.o V3.print l.dir

let to_string l = 
  Format.fprintf Format.str_formatter "%a" print l; 
  Format.flush_str_formatter ()

