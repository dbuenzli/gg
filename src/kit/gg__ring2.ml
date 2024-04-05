(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg

(* Linear rings *)

type t = P2.t list
let of_pts pts = pts
let empty = []

(* Properties *)

let pts = Fun.id
let area = function
| [] | [_] | [_; _] -> 0.
| pt :: (_ :: _ as pts) ->
    (* XXX this is not robust see p. 245 of gds for cg.  *)
    let[@inline] det p0 p1 = (V2.x p0 *. V2.y p1) -. (V2.y p0 *. V2.x p1) in
    let rec loop acc first prev = function
    | [] -> 0.5 *. (acc +. (det[@inlined]) first prev)
    | pt :: pts ->
        (loop[@tailcall]) (acc +. (det[@inlined]) pt prev) first pt pts
    in
    loop 0. pt pt pts

let centroid pts =
  let rec loop n cx cy = function
  | [] -> let n = float n in V2.v (cx /. n) (cy /. n)
  | pt :: pts  -> (loop[@tailcall]) (n + 1) (cx +. V2.x pt) (cy +. V2.y pt) pts
  in
  loop 0 0. 0. pts

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

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
