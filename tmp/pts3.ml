(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

type access = [ `R | `W ]
type 'a acc = 
    { count : 'a -> int; 
      el : 'a -> int -> V3.t; 
      set_el : 'a -> int -> V3.t -> unit }

(** The type expressed by the three types below is
    forall 'a. exists 'i . { acc : 'i acc; v : 'i } *)

type 'i impl = { acc : 'i acc; v : 'i }
type 's scope = { bind : 'i. 'i impl -> 's }
type +'a t = { open_ : 's. 's scope -> 's } constraint 'a = [< access ]  

let make acc access v = { open_ = fun scope -> scope.bind { acc = acc; v = v }}

let count pts = pts.open_ { bind = fun i -> i.acc.count i.v }
let el pts e = pts.open_ { bind = fun i -> i.acc.el i.v e }
let set_el pts e = pts.open_ { bind = fun i -> i.acc.set_el i.v e }

let iter f pts = pts.open_ { bind = fun i ->
    for e = 0 to i.acc.count i.v - 1 do f (i.acc.el i.v e) done }

let iteri f pts = pts.open_ { bind = fun i ->
    for e = 0 to i.acc.count i.v - 1 do f e (i.acc.el i.v e) done }

let set_map f pts pts' = pts.open_ { bind = fun i -> 
  pts'.open_ { bind = fun i' -> 
    for e = 0 to i.acc.count i.v - 1 do 
      i'.acc.set_el i'.v e (f (i.acc.el i.v e)) done }}

let set_mapi f pts pts' = pts.open_ { bind = fun i -> 
  pts'.open_ { bind = fun i' -> 
    for e = 0 to i.acc.count i.v - 1 do 
      i'.acc.set_el i'.v e (f e (i.acc.el i.v e)) done }}
    
let fold_left f x pts = pts.open_ { bind = fun i -> 
  let r = ref x in
  for e = 0 to i.acc.count i.v - 1 do
    r := f !r (i.acc.el i.v e)
  done;
  !r }
  
let fold_right f pts x = pts.open_ { bind = fun i ->
  let r = ref x in
  for e = i.acc.count i.v - 1 downto 0 do
    r := f (i.acc.el i.v e) !r
  done;
  !r }

let iter2 f pts pts' = pts.open_ { bind = fun i ->
  pts'.open_ { bind = fun i' ->
    for e = 0 to i.acc.count i.v - 1 do 
      f (i.acc.el i.v e) (i'.acc.el i'.v e) done }}


let set_map2 f pts pts' pts'' = pts.open_ { bind = fun i -> 
  pts'.open_ { bind = fun i' ->
    pts''.open_ { bind = fun i'' -> 
      for e = 0 to i.acc.count i.v - 1 do 
	i''.acc.set_el i''.v e (f (i.acc.el i.v e) (i'.acc.el i'.v e)) done }}}


let fold_left2 f x pts pts' = pts.open_ { bind = fun i -> 
  pts'.open_ { bind = fun i' -> 
    let r = ref x in
    for e = 0 to i.acc.count i.v - 1 do
      r := f !r (i.acc.el i.v e) (i'.acc.el i'.v e)
    done;
    !r }}
  
let fold_right2 f pts pts' x = pts.open_ { bind = fun i ->
  pts'.open_ { bind = fun i' -> 
    let r = ref x in
    for e = i.acc.count i.v - 1 downto 0 do
      r := f (i.acc.el i.v e) (i'.acc.el i'.v e) !r
    done;
    !r }}


