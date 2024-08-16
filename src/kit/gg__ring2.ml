(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg

(* Linear rings *)

type t = P2.t list (* XXX maybe switch to arrays. *)
let of_pts pts = pts
let empty = []

(* Properties *)

let pts = Fun.id
let area = function
| [] | [_] | [_; _] -> 0.
| pts ->
    let a = ref 0. and acc = ref pts and last = ref false in
    while !acc <> [] do match !acc with
    | p0 :: (p1 :: _ as acc') ->
        (* XXX this is not robust see p. 245 of
             Geometric data structures for computer graphics. *)
        let x0 = P2.x p0 and y0 = P2.y p0 in
        let x1 = P2.x p1 and y1 = P2.y p1 in
        let w = (x0 *. y1) -. (x1 *. y0) in
        a := !a +. w;
        acc := acc';
    | [p] ->
        if !last then acc := [] else (last := true ; acc := [p; List.hd pts])
    | [] -> assert false
    done;
    0.5 *. !a

let centroid = function
| [] | [_] | [_; _] -> P2.o
| pts ->
  (* https://paulbourke.net/geometry/polygonmesh/centroid.pdf *)
  let cx = ref 0. and cy = ref 0. and a = ref 0. in
  let acc = ref pts and last = ref false in
  while !acc <> [] do match !acc with
  | p0 :: (p1 :: _ as acc') ->
      let x0 = P2.x p0 and y0 = P2.y p0 in
      let x1 = P2.x p1 and y1 = P2.y p1 in
      let w = (x0 *. y1) -. (x1 *. y0) in
      a := !a +. w;
      cx := !cx +. (x0 +. x1) *. w;
      cy := !cy +. (y0 +. y1) *. w;
      acc := acc';
  | [p] ->
      if !last then acc := [] else (last := true ; acc := [p; List.hd pts])
  | [] -> assert false
  done;
  let a = 0.5 *. !a in
  let wa = 1. /. (6. *. a) in
  P2.v (wa *. !cx) (wa *. !cy)

let box pts =
  if pts = [] then Box2.empty else
  let rec loop xmin ymin xmax ymax = function (* XXX rewrite, this boxes *)
  | [] -> Box2.of_pts (V2.v xmin ymin) (V2.v xmax ymax)
  | pt :: pts ->
      let px = V2.x pt and py = V2.y pt in
      let xmin = Float.min xmin px and ymin = Float.min ymin py in
      let xmax = Float.max xmax px and ymax = Float.max ymax py in
      (loop[@tailcall]) xmin ymin xmax ymax pts
  in
  let fmax = Float.max_float and fmin = ~-. Float.max_float in
  loop fmax fmax fmin fmin pts

(* Predicates *)

let is_empty r = r = []
let mem pt = function
| [] -> false
| [p] -> V2.equal pt p
| p :: ps ->
    let[@inline] is_inside inside x y xj yj xi yi =
      if ((yi <= y && y < yj) || (yj <= y && y < yi)) &&
         (x < (xj -. xi) *. (y -. yi) /. (yj -. yi) +. xi)
      then not inside else inside
    in
    let rec loop first inside x y xj yj = function
    | [] ->
        let xi = V2.x first and yi = V2.y first in
        (is_inside[@inlined]) inside x y xj yj xi yi
    | p :: ps ->
        let xi = V2.x p and yi = V2.y p in
        let inside = (is_inside[@inlined]) inside x y xj yj xi yi in
        loop first inside x y xi yi ps
    in
    loop p false (V2.x pt) (V2.y pt) (V2.x p) (V2.y p) ps

(* Transforming *)

let swap_orientation pts = List.rev pts

(* Traversals *)

let fold_pts f c acc = List.fold_left (Fun.flip f) acc c
let fold_segs f c acc = match c with
| [] -> acc
| [p] -> f p p acc
| p :: (_ :: _ as ps) ->
    let rec loop f acc first prev = function
    | [] -> f prev first acc
    | p :: ps -> (loop[@tailcall]) f (f prev p acc) first p ps
    in
    loop f acc p p ps
