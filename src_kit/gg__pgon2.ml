(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg

(* Resizable array. *)

module Rarray = struct
  type 'a t =
    { nil : 'a;
      mutable els : 'a array;
      mutable max : int; (* index of last element of [els]. *) }

  let make nil ~size = { nil; els = Array.make size nil; max = -1 }
  let grow a =
    let len = a.max + 1 in
    let els' = Array.make (2 * len) a.nil in
    Array.blit a.els 0 els' 0 len; a.els <- els'

  let length a = a.max + 1
  let add_last a v =
    let max = a.max + 1 in
    if max = Array.length a.els then grow a;
    a.max <- max; a.els.(max) <- v
end

(* Heap priority queue. Classical imperative implementation. Note that given
   the one shot use in this module we don't bother with shrinking. *)

module type HEAPABLE = sig type t val compare : t -> t -> int val nil : t end
module Heap (El : HEAPABLE) = struct
  let[@inline] heap_compare h i i' = El.compare h.(i) h.(i')
  let[@inline] heap_swap h i i' = let v = h.(i) in h.(i) <- h.(i'); h.(i') <- v
  let rec heap_up h i =
    if i = 0 then () else
    let p = (i - 1) / 2 in (* parent index. *)
    if heap_compare h i p < 0 then (heap_swap h i p; heap_up h p)

  let rec heap_down h max i =
    let start = 2 * i in
    let l = start + 1 in (* left child index. *)
    let r = start + 2 in (* right child index. *)
    if l > max then () (* no child, stop *) else (* find smallest child k. *)
    let k = if r > max then l else (if heap_compare h l r < 0 then l else r) in
    if heap_compare h i k > 0 then (heap_swap h i k; heap_down h max k)

  type t = El.t Rarray.t (* array of elements with the heap invariant *)

  let make ~size = Rarray.make El.nil ~size
  let peek (h : t) = if h.max < 0 then None else Some (h.els.(0))
  let take (h : t) =
    if h.max < 0 then None else
    let v = Some h.els.(0) in
    let last = h.els.(h.max) in
    h.els.(h.max) <- El.nil; h.max <- h.max - 1; h.els.(0) <- last;
    heap_down h.els h.max 0;
    v

  let add (h : t) e =
    let max = h.max + 1 in
    if max = Array.length h.els then Rarray.grow h;
    h.max <- max; h.els.(h.max) <- e;
    heap_up h.els h.max
end

(* Contours *)

module Contour = struct
  type t = P2.t list
  let is_empty c = c = []
  let of_seg_pts pts = pts
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

  let centroid ps =
    let rec loop n cx cy = function
    | [] -> let n = float n in V2.v (cx /. n) (cy /. n)
    | p :: ps  -> (loop[@tailcall]) (n + 1) (cx +. V2.x p) (cy +. V2.y p) ps
    in
    loop 0 0. 0. ps

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

  let area = function
  | [] | [_] | [_; _] -> 0.
  | p :: (_ :: _ as ps) ->
      (* XXX this is not robust see p. 245 of gds for cg.  *)
      let[@inline] det p0 p1 = (V2.x p0 *. V2.y p1) -. (V2.y p0 *. V2.x p1) in
      let rec loop acc first prev = function
      | [] -> 0.5 *. (acc +. (det[@inlined]) first prev)
      | p :: ps -> (loop[@tailcall]) (acc +. (det[@inlined]) p prev) first p ps
      in
      loop 0. p p ps

  let box ps =
    if ps = [] then Box2.empty else
    let rec loop xmin ymin xmax ymax = function
    | [] -> Box2.of_pts (V2.v xmin ymin) (V2.v xmax ymax)
    | p :: ps ->
        let px = V2.x p and py = V2.y p in
        let xmin = Float.min xmin px and ymin = Float.min ymin py in
        let xmax = Float.max xmax px and ymax = Float.max ymax py in
        (loop[@tailcall]) xmin ymin xmax ymax ps
    in
    let fmax = Float.max_float and fmin = ~-. Float.max_float in
    loop fmax fmax fmin fmin ps

  let swap_orientation ps = List.rev ps
end

(* Polygons *)

let min_max_coords cs = (* assert (cs <> []) *)
  let rec contour_loop cs xmin ymin xmax ymax = function
  | [] -> (loop[@tailcall]) xmin ymin xmax ymax cs
  | p :: ps ->
      let px = V2.x p and py = V2.y p in
      let xmin = Float.min xmin px and ymin = Float.min ymin py in
      let xmax = Float.max xmax px and ymax = Float.max ymax py in
      (contour_loop[@tailcall]) cs xmin ymin xmax ymax ps
  and loop xmin ymin xmax ymax = function
  | [] -> (P2.v xmin ymin), (P2.v xmax ymax)
  | c :: cs -> (contour_loop[@tailcall]) cs xmin ymin xmax ymax c
  in
  let fmax = Float.max_float and fmin = ~-. Float.max_float in
  loop fmax fmax fmin fmin cs

let min_max_nil = P2.o, P2.o

type t =
  { cs : Contour.t list;
    (* Note. We do not store this as a Box2.t value because we need a
       precise Box2.maxx for the boolean operation optimizations
       cases. If we get an under approximation because of the minx
       +. (maxx - minx) computation the algo returns bogus results
       (some events that need to be in the list resulting from the
       sweep may not be in).

       We use a mutable field instead of a lazy value. In `js_of_ocaml` that
       allows us pass polygons through the structured clone algorithm. *)
    mutable min_max : (P2.t * P2.t) option }

let empty = { cs = []; min_max = None }
let v cs =
  let cs = List.filter (fun c -> c <> []) cs in
  if cs = [] then empty else { cs; min_max = None }

let is_empty p = p.cs = []

let min_max p = match p.min_max with
| Some min_max -> min_max
| None ->
    let min_max = min_max_coords p.cs in
    p.min_max <- Some min_max; min_max

let box p =
  if is_empty p then Box2.empty else
  let min, max = min_max p in
  Box2.of_pts min max

let fold_contours f p acc = List.fold_left (Fun.flip f) acc p.cs

(* Boolean operations.

   The algorithm here is based on:

     A new algorithm for computing Boolean operation on polygons.
     Francisco Martínez et al. https://doi.org/10.1016/j.cageo.2008.08.009

     A simple algorithm for Boolean operations on polygons.
     Francisco Martínez et al. https://doi.org/10.1016/j.advengsoft.2013.04.004

   The second paper improves the first one and is the one we implement. Both
   papers are useful to understand what is going on. This implementation is
   drawn from the public domain reference C++ implementation found at
   https://www4.ujaen.es/~fmartin/bool_op.html. However note that the version
   found there as of 2022 is not bug free. *)

type bool_op = Union | Diff | Inter | Xor
type polygon = Subject | Clipping (* left or right argument of the bool ops *)
type overlap = (* For overlap information see §7.2 of the first paper. *)
| No | Not_contributing | Same_inout | Different_inout

(* Sweep line events. Sweep events occur at the endpoints of polygon
   segments and their intersections. *)

module Event = struct
  type t =
    { polygon : polygon; (* polygon to which the segment belongs *)
      pt : P2.t; (* endpoint of the segment. *)
      mutable other : t; (* event for the other endpoint of the segment *)
      mutable is_left : bool; (* [pt] is the left endpoint of the segment. *)
      (* Information set during the sweep see §4 of the paper. *)
      mutable overlap : overlap; (* See §7.2 of the first paper. *)
      mutable inout : bool; (* [true] if a ray shot from below the polygon
                               gets out [polygon]. *)
      mutable other_poly_inout : bool; (* same as [inout] but for the closet
                                          edge below of the other polygon. *)
      mutable in_result : bool; (* [true] if endpoint belongs to the result *)
      mutable below_in_result : t; (* points to the closest edge below also
                                      in the result. [nil] if none. *)
      mutable resort : bool;
      (* [true] if the event needs to be resorted when it gets out of the
         prioriy queue because [is_left] was mutated during division. a*)
      (* For connecting the results see §5 of the paper. *)
      mutable sort_index : int;
      mutable contour_id : int;
      mutable result_inout : bool; }

  let ppv2 = V2.pp_f (fun ppf v -> Format.fprintf ppf "%.17f" v)
  let pp_dump ppf e =
    Format.fprintf ppf "@[%d:%s:%b %a - %d %a@]"
      e.sort_index (if e.is_left then "L" else "R")
      e.in_result ppv2 e.pt e.other.sort_index ppv2 e.other.pt

  let rec nil =
    (* nil event for initializing data structures. Careful, compare with
       (==), (=) diverges :-) *)
    { polygon = Subject; pt = P2.o; other = nil; is_left = true;
      overlap = No; inout = false; other_poly_inout = false;
      in_result = false; below_in_result = nil; resort = false; sort_index = -1;
      contour_id = -1; result_inout = false; }

  let v ~polygon ~pt ~is_left =
    { polygon; pt; other = nil; is_left; overlap = No; inout = false;
      other_poly_inout = false; in_result = false; below_in_result = nil;
      resort = false; sort_index = -1; contour_id = -1; result_inout = false; }

  let is_seg_vertical e = Float.equal (P2.x e.pt) (P2.x e.other.pt)

  let p2_orient = P2.orient

  let is_seg_below ~seg:e ~pt = (* [true] if segment [e] is below [pt] *)
    if e.is_left
    then p2_orient e.pt e.other.pt pt > 0.
    else p2_orient e.other.pt e.pt pt > 0.

  let is_seg_above ~seg:e ~pt = not (is_seg_below ~seg:e ~pt)

  let segs_collinear e0 e1 =
    (* The reference implementation used the following which is not a
       robust collinearity test. We use the relative epsilon collinearity
       found in Gg.P2.seg_inter. It fixes some of our tests. *)
    (*  P2.orient e0.pt e0.other.pt e1.pt = 0. &&
        P2.orient e0.pt e0.other.pt e1.other.pt = 0. *)
    let eps2 = 1e-8 in
    let p0 = e0.pt and p1 = e0.other.pt in
    let q0 = e1.pt and q1 = e1.other.pt in
    let d0x = P2.x p1 -. P2.x p0 and d0y = P2.y p1 -. P2.y p0 in
    let d1x = P2.x q1 -. P2.x q0 and d1y = P2.y q1 -. P2.y q0 in
    let kross = (d0x *. d1y) -. (d0y *. d1x) in
    let sqr_kross = kross *. kross in
    let sqr_len0 = (d0x *. d0x) +. (d0y *. d0y) in
    let sqr_len1 = (d1x *. d1x) +. (d1y *. d1y) in
    if sqr_kross > eps2 *. sqr_len0 *. sqr_len1 then false else
    let ex = P2.x q0 -. P2.x p0 and ey = P2.y q0 -. P2.y p0 in
    let sqr_lene = (ex *. ex) +. (ey *. ey) in
    let kross = (ex *. d0y) -. (ey *. d0x) in
    let sqr_kross = kross *. kross in
    if sqr_kross > eps2 *. sqr_len0 *. sqr_lene then false else true

  (* Order for the priority queue. Orders first from left to right, then
     bottom to top, then right endpoints before left and other special cases. *)
  let compare e0 e1 =
    let c = Float.compare (P2.x e0.pt) (P2.x e1.pt) in
    if c <> 0 then c (* smallest x processed before *) else
    let c = Float.compare (P2.y e0.pt) (P2.y e1.pt) in
    if c <> 0 then c (* smallest y processed before *) else
    (* e0.pt and e1.pt coincide. *)
    let c = Bool.compare e0.is_left e1.is_left in
    if c <> 0 then c (* right endpoint processed before left *) else
    if segs_collinear e0 e1 then
      (* Not exactly sure whether this is useful, I think it does
         label clipping as non-contributing on overlaps but I don't think
         it matters. *)
      Stdlib.compare e0.polygon e1.polygon (* Subject before Clipping *)
    else
    if is_seg_above ~seg:e0 ~pt:e1.other.pt
    then 1 else -1 (* event of bottom seg before *)

  let in_result op e = match e.overlap with
  | No ->
      begin match op with
      | Inter -> not e.other_poly_inout
      | Union -> e.other_poly_inout
      | Diff ->
          (e.polygon = Subject && e.other_poly_inout) ||
          (e.polygon = Clipping && not e.other_poly_inout)
      | Xor -> true
      end
  | Same_inout -> (match op with Inter | Union -> true | _ -> false)
  | Different_inout -> (match op with Diff -> true | _ -> false)
  | Not_contributing -> false

  (* These mutations do not affect the order in [q] or [s].
     [below] is the event for the segment just below in the sweep line status.
     Mutating these fields has no effect on the comparison done by
     [Event.compare] and [Status.seg_compare]. *)
  let set_fields op e ~below = match below with
  | None ->
      e.inout <- false;
      e.other_poly_inout <- true; (* We are outside the other *)
      e.in_result <- in_result op e;
  | Some below ->
      let inout, other_poly_inout = match e.polygon = below.polygon with
      | true -> not below.inout, below.other_poly_inout
      | false ->
          not below.other_poly_inout (* that other is ours *),
          if is_seg_vertical below then not below.inout else below.inout
      in
      let below_in_result =
        match not below.in_result || is_seg_vertical below with
        | true -> below.below_in_result | false -> below
      in
      e.inout <- inout;
      e.other_poly_inout <- other_poly_inout;
      e.below_in_result <- below_in_result;
      e.in_result <- in_result op e

  let filter_and_sort evs =
    (* Results needs to be resorted because of overlaps. We may
       also have dupes in [evs] because of reprocesses. *)
    let module Eset =
      Set.Make (struct type nonrec t = t let compare = compare end)
    in
    let add_to_result acc e =
      if (e.is_left && e.in_result) || (not e.is_left && e.other.in_result)
      then Eset.add e acc else acc
    in
    let evs = List.fold_left add_to_result Eset.empty evs in
    let r = Array.make (Eset.cardinal evs) nil in
    let add e i =
      (* XXX the original algo swaps if r.(i).pos and r.(i).other.pos
         if not r.(i).is_left. It's unclear to me why this is done and
         therefore not done here. *)
      e.sort_index <- i; r.(i) <- e; i + 1
    in
    ignore (Eset.fold add evs 0); r
end

(* Sweep line status.

   This holds, via an ordered set of [is_left] Event.t values, all
   the segments that intersect the sweep line. The general idea is to orders
   the segments from bottom to top, but there are many special cases to
   treat. *)
module Status = struct
  (* Order for the sweep line status. Orders the is_left events first from
     bottom to top and then other special cases. *)
  let seg_compare (e0 : Event.t) (e1 : Event.t) =
    assert (e0.is_left && e1.is_left);
    if e0 == e1 then 0 else
    if Event.segs_collinear e0 e1 then begin
      (* N.B. the ref impl. fails on these things  *)
      let c = Event.compare e0 e1 in
      let ll, lr = if c < 0 then e0, e1 else e1, e0 in
      if V2.x ll.other.pt < V2.x lr.other.pt then
        (* lr prolonges ll, let the prolongation be above or below
           according to the segment's direction. *)
         let c' = Float.compare (V2.y ll.pt)  (V2.y lr.pt) in
         if c' <> 0 then c' else (* Horizontal, use ref implem. *)
         let c' = Stdlib.compare e0.polygon e1.polygon in
         if c' <> 0 then c' else c
      else
      (* Like in the reference implem. *)
      let c' = Stdlib.compare e0.polygon e1.polygon in
      if c' <> 0 then c' else c
    end else
    if V2.equal e0.pt e1.pt (* When left coincide use right point to sort *)
    then (if Event.is_seg_below ~seg:e0 ~pt:e1.other.pt then -1 else 1) else
    if Float.equal (P2.x e0.pt) (P2.x e1.pt)
    then
      let c = Float.compare (P2.y e0.pt) (P2.y e1.pt) in
      if c <> 0 then c else Event.compare e0 e1 (* XXX *)
    else
    (* XXX no longer understand this. *)
    if Event.compare e0 e1 < 0 (* e0 before e1 in the queue ? *)
    then (if Event.is_seg_below ~seg:e0 ~pt:e1.pt then -1 else 1)
    else (if Event.is_seg_above ~seg:e1 ~pt:e0.pt then -1 else 1)

  include Set.Make (struct type t = Event.t let compare = seg_compare end)
  let assert' s = for_all (fun e -> e.is_left) s
  let above e = find_first_opt (fun e' -> seg_compare e' e > 0)
  let below e = find_last_opt (fun e' -> seg_compare e' e < 0)
end

module Equeue = struct
  include Heap (Event)

  let rec drain q =
    let rec loop q acc = match take q with
    | None -> List.rev acc | Some e -> loop q (e :: acc)
    in
    loop q []

  let heap_take = take
  let rec take q = match heap_take q with
  | None -> None
  | Some e when e.Event.resort -> e.resort <- false; add q e; take q
  | _ as r -> r

  let add_segments q ~polygon p =
    let add_seg p0 p1 q =
      if V2.equal p0 p1 then (* skip degenerate segment *) q else
      let p0_is_left = V2.compare p0 p1 < 0 in
      let e0 = Event.v ~polygon ~pt:p0 ~is_left:p0_is_left in
      let e1 = Event.v ~polygon ~pt:p1 ~is_left:(not p0_is_left) in
      e0.Event.other <- e1; e1.Event.other <- e0;
      add q e0; add q e1;
      q
    in
    let add_contour c q = Contour.fold_segs add_seg c q in
    ignore (fold_contours add_contour p q)

  let make ~subject:p0 ~clipping:p1 =
    let q = make ~size:1024 in
    add_segments q ~polygon:Subject p0; add_segments q ~polygon:Clipping p1; q

  let divide_seg q e ~at:pt = (* segment e-e.other becomes e-r and l-e.other *)
    assert (e.Event.is_left);
    let el = e and er = e.Event.other in
    let r = Event.v ~polygon:e.Event.polygon ~pt ~is_left:false in
    let l = Event.v ~polygon:e.Event.polygon ~pt ~is_left:true in
    el.other <- r; r.other <- el; l.other <- er; er.other <- l;
    (* Fix processing order in case of roundings error. *)
    if not (Event.compare l l.other <= 0)
    (* [l.other] is still in the priority queue we ask to resort
       it when it pops out from the queue. *)
    then (l.is_left <- false; l.other.is_left <- true; l.other.resort <- true);
    let e_order_swap =
      if not (Event.compare r.other r <= 0)
      then (r.is_left <- true; (* e *) r.other.is_left <- false; true)
      else false
    in
    add q r; add q l;
    (* We return [l] to check for seg. sort changes *)
    l, e_order_swap

  let handle_intersection_pt s q ~below b0 b1 ~above a0 a1 ~pt:i =
    let eq_left = V2.equal b0 a0 and eq_right = V2.equal b1 a1 in
    if eq_left || eq_right then (* segs: > or <, nothing to do *) Ok s else
    (* A segment only gets divided if [i] is not one of its endpoints. *)
    let divide_below = not (V2.equal b0 i) && not (V2.equal b1 i) in
    let divide_above = not (V2.equal a0 i) && not (V2.equal a1 i) in
    (* If we mutate the events their sort can change. Remove them before. *)
(*    assert (Status.mem below s && Status.mem above s); *)
    let s = if divide_below then Status.remove below s else s in
    let s = if divide_above then Status.remove above s else s in
    let bl, below_order_swap =
      if divide_below then divide_seg q below ~at:i else Event.nil, false
    in
    let al, above_order_swap =
      if divide_above then divide_seg q above ~at:i else Event.nil, false
    in
    let s =
      if above_order_swap ||
         (divide_below && not divide_above && Status.seg_compare bl above > 0)
         (* New segment [bl] is above [above], need to reprocess it. *)
      then (add q above; s)
      else (if divide_above then (* re-add *) Status.add above s else s)
    in
    let s =
      if below_order_swap ||
         (divide_above && not divide_below && Status.seg_compare al below < 0)
         (* New segment [al] is below [below], need to reprocess it. *)
      then (add q below; s)
      else (if divide_below then (* re-add *) Status.add below s else s)
    in
    (* Note that if [divide_above] and [divide_below] are both true. [below] and
       [above] no longer exist as such. For the new segments, those that
       start with [a0] and [b0] should be sorted correctly relative to each
       other by the virtue of [below] and [above] being sorted. The other
       ones will be sorted in the future and their left points should be on the
       right of those segments starting in [a0] and [b0]. So nothing
       special has to be done. *)
    Ok s

  let update_inout op seg s =
    (* These mutations do not affect order in [q] or [s]. *)
    let below = Status.below seg s in
    Event.set_fields op seg ~below

  let update_overlap_fields ~keep ~drop =
    (* These mutations do not affect order in [q] or [s]. [keep] and [drop]
       are the same segment, we need only one in the result.  *)
    drop.Event.overlap <- Not_contributing;
    keep.Event.overlap <-
      if keep.Event.inout = drop.Event.inout
      then Same_inout else Different_inout

  let handle_overlap op s q ~below ~above =
    (* Overlap needs to handle these five cases:
       *......*  *....*      *....*    *..*        *......*
       *------*  *------*  *------*  *------*  *------*

       XXX The update_inout is still a bit unclear. Also should we
       systematically resort the actual overlap.
    *)
    let eq_left = V2.equal below.Event.pt above.Event.pt in
    let eq_right = V2.equal below.Event.other.pt above.Event.other.pt in
    if eq_left && eq_right then begin (* full overlap *)
      update_overlap_fields ~keep:below ~drop:above;
      update_inout op below s; update_inout op above s;
      s
    end else if eq_left then begin (* longest seg needs to be cut *)
      let keep, to_cut =
        if Event.compare below.other above.other < 0
        then below, above else above, below
      in
      let s = Status.remove to_cut s (* will mutate *) in
      let _new_seg, swap = divide_seg q to_cut ~at:keep.other.pt in
      let s = Status.add (if swap then to_cut.other else to_cut) s in
      update_overlap_fields ~keep ~drop:to_cut;
      update_inout op below s; update_inout op above s;
      s
    end else if eq_right then begin (* longest seg needs to be cut *)
      let keep, to_cut =
        if Event.compare below above < 0
        then above, below else below, above
      in
      let s = Status.remove to_cut s (* will mutate *) in
      (* XXX new_seg will get into the queue not sure that's a good idea. *)
      let new_seg, swap = divide_seg q to_cut ~at:keep.pt in
      let s = Status.add (if swap then to_cut.other else to_cut) s in
      new_seg.inout <- to_cut.inout; (* XXX maybe divide_seg should do that. *)
      update_overlap_fields ~keep ~drop:new_seg;
      s
    end else begin
      (* either one segment contains the other or partial overlap *)
      let leftmost, left =
        if Event.compare below above < 0
        then below, above else above, below
      in
      let rightmost, right =
        if Event.compare below.other above.other > 0
        then below, above else above, below
      in
      if leftmost == rightmost then begin (* leftmost contains left *)
        let keep, to_cut_twice = left, leftmost in
        (* XXX new_segs will get into the queue not sure that's a good idea. *)
        let s = Status.remove to_cut_twice s (* will mutate *) in
        let new_seg, swap = divide_seg q to_cut_twice ~at:keep.pt in
        new_seg.inout <- to_cut_twice.inout;
        update_overlap_fields ~keep ~drop:new_seg;
        let _new_seg, _swap = divide_seg q new_seg ~at:keep.other.pt in
        let to_cut_twice = if swap then to_cut_twice.other else to_cut_twice in
        let s = Status.add to_cut_twice s in
        s
      end else begin
        let s = Status.remove leftmost s (* will mutate *) in
        let s = Status.remove left s (* will mutate *) in
        let leftmost_at = left.pt and left_at = leftmost.other.pt in
        let new_seg, lefmost_swap = divide_seg q leftmost ~at:leftmost_at in
        let _new_seg, left_swap = divide_seg q left ~at:left_at in
        new_seg.inout <- leftmost.inout;
        update_overlap_fields ~keep:left ~drop:new_seg;
        let leftmost = if lefmost_swap then leftmost.other else leftmost in
        let left = if left_swap then left.other else left in
        let s = Status.add leftmost s in
        let s = Status.add left s in
        s
      end
    end

  let handle_intersection op s q ~below ~above =
    assert (Status.mem below s && Status.mem above s);
    assert (below.Event.is_left && above.Event.is_left);
    let b0 = below.Event.pt and b1 = below.other.pt in
    let a0 = above.Event.pt and a1 = above.other.pt in
    let r = match P2.seg_inter ~p0:b0 ~p1:b1 ~q0:a0 ~q1:a1 () with
    | `None -> Ok s
    | `Pt pt -> handle_intersection_pt s q ~below b0 b1 ~above a0 a1 ~pt
    | `Seg (i0, i1) ->
        if below.Event.polygon = above.Event.polygon
        then Error s (* self-overlap not supported by the algo *)
        else Ok (handle_overlap op s q ~below ~above)
    in
    let s = match r with Ok s -> s | Error s -> s in
    assert (Status.assert' s);
    r
end

module Sweep = struct
  (* The sweep. Finds all the intersection and attributes segments to
     the result. *)

  let[@inline] left_pt err op q s e =
    let s = Status.add e s in
    let s = match Status.above e s with
    | None -> s
    | Some above ->
        Event.set_fields op e ~below:(Status.below e s);
        match Equeue.handle_intersection op s q ~below:e ~above with
        | Ok s -> s | Error s -> err := true; s
    in
    if not e.is_left (* order swap, will be reprocessed *) then s else
    let below = Status.below e s (* note that [s] may have changed here *) in
    Event.set_fields op e ~below;
    match below with
    | None -> s
    | Some b ->
        let s = match Equeue.handle_intersection op s q ~below:b ~above:e with
        | Ok s -> s | Error s -> err := true; s
        in
        if not e.is_left then s else
        ((* Removes a panic from the test suite, but this should be done
            in a principled way in handle_intersection since it adds another*)
          Event.set_fields op e ~below:(Status.below e s); s)

  let[@inline] right_pt err op q s e =
    let e = e.Event.other (* work on the left end which is the one in [s] *) in
    let below = Status.below e s and above = Status.above e s in
    let s = Status.remove e s in
    match below, above with
    | Some below, Some above ->
        begin match Equeue.handle_intersection op s q ~below ~above with
        | Ok s -> s | Error s -> err := true; s
        end
    | _ -> s

  let[@inline] maxx p =
    if is_empty p then -. min_float else P2.x (snd (min_max p))

  let events op p0 p1 =
    let rec loop err op b0maxx minmaxx evs q s = match Equeue.take q with
    | None -> evs
    | Some e ->
        assert (e == e.other.other);
(*        assert (if e.is_left then not (Status.mem e s) else true); *)
        match op with
        (* Optimizations. See point 2. in §8 of the paper. Note, we
           need to make sure we have all the events in [evs] this means we
           need a precise minmaxx and b0max. Alternatively we could
           drain the queue in [evs]. *)
        | Inter when P2.x e.pt > minmaxx -> evs
        | Diff when P2.x e.pt > b0maxx -> evs
        | _ ->
            let evs = e :: evs in
            let s = match e.is_left with
            | true -> (left_pt[@inlined]) err op q s e
            | false -> (right_pt[@inlined]) err op q s e
            in
            (loop[@tailcal]) err op b0maxx minmaxx evs q s
    in
    let b0maxx = maxx p0 in
    let minmaxx = Float.min b0maxx (maxx p1) in
    let q = Equeue.make ~subject:p0 ~clipping:p1 in
    let errs = ref false in
    let evs = loop errs op b0maxx minmaxx [] q Status.empty in
    if !errs then Error evs else Ok evs

  let debug_stepper op p0 p1 = (* XXX mostly copies [events]. *)
    let b0maxx = maxx p0 in
    let minmaxx = Float.min b0maxx (maxx p1) in
    let q = Equeue.make ~subject:p0 ~clipping:p1 in
    let err = ref false in
    let s = ref Status.empty in
    let step () = match Equeue.take q with
    | None -> None
    | Some e ->
        match op with
        | Inter when P2.x e.pt > minmaxx -> None
        | Diff when P2.x e.pt > b0maxx -> None
        | _ ->
            let s' = if e.is_left then Status.add e !s else !s in
            let below = Status.below (if e.is_left then e else e.other) s' in
            let above = Status.above (if e.is_left then e else e.other) s' in
            s := (match e.is_left with
              | true -> (left_pt[@inlined]) err op q !s e
              | false -> (right_pt[@inlined]) err op q !s e);
            Some (below, e, above, Status.elements s')
    in
    step

  let debug_result ?(filtered = true) op p0 p1 = match events op p0 p1 with
  | Ok evs | Error evs ->
      if filtered then Array.to_list (Event.filter_and_sort evs) else evs
end

module Topology = struct
  (* Final part of the algorithm finds contours and their orientation
     in the soup of result segments.

     TODO for now we don't compute the holes direction but interpreting
     the result with the even-odd rule should yield correct results. *)

  type contour =
    { mutable depth : int;
      mutable holeof : int;
      mutable holes : int list;
      mutable is_external : bool;
      mutable contour : P2.t list; }

  let contour () =
    { depth = -1; holeof = -1; holes = []; is_external = true; contour = [] }

  let nil = contour ()

  type t = contour Rarray.t
  let make ~size = Rarray.make nil ~size
  let get cid (cs : t) = assert (0 <= cid && cid <= cs.max); cs.els.(cid)
  let new' (cs : t) =
    let c = contour () in Rarray.add_last cs c; Rarray.length cs - 1, c

  let final_polygon (css : t) =
    let cs = ref [] and hs = ref [] in
    for i = css.max downto 0 do
      cs := css.els.(i).contour :: !cs; hs := css.els.(i).holes :: !hs
    done;
    v !cs, !hs

  let assigned e = e.Event.contour_id <> -1

  let init_contour errs cs start =
    let contour_id, c as r = new' cs in
    let below = start.Event.below_in_result in
    if below == Event.nil then r else
    if not (assigned below)
    then (errs := !errs ^ Format.asprintf "UNASSIGNED BELOW!@."; r) else
    let belowc = get below.contour_id cs in
    if below.result_inout then
      (belowc.holes <- contour_id :: belowc.holes;
       c.holeof <- below.contour_id;
       c.depth <- belowc.depth + 1;
       c.is_external <- false;
       r)
    else
    if not belowc.is_external then
      (let parent = get belowc.holeof cs in
       parent.holes <- contour_id :: parent.holes;
       c.holeof <- belowc.holeof;
       c.depth <- belowc.depth;
       c.is_external <- false;
       r)
    else r

  let find_kont errs ~panic r (start : Event.t) =
    let rec find_down r i =
      (* N.B. theoretically this should always find a non-assigned event
         with point [start.pt]. But it's a bit unclear what happens with
         our non-failing behaviour on overlapping self-edges. So if we
         hit the bottom we simply return [panic] which stops the contour
         walk process (see value given at calling point). *)
      if i < 0
      then (errs :=
              !errs ^ (Format.asprintf "PANIC! %d %a@."
                         start.Event.sort_index Event.ppv2 start.Event.pt);
            panic) else
      if assigned r.(i) then find_down r (i - 1) else r.(i)
    in
    let rec find_up r max i start =
      if i <= max && V2.equal r.(i).Event.pt start.Event.pt
      then (if assigned r.(i) then find_up r max (i + 1) start else r.(i))
      else find_down r (start.Event.sort_index - 1)
    in
    find_up r (Array.length r - 1) (start.sort_index + 1) start

  let next_contour errs cs evs start =
    let rec loop errs evs cid c (start : Event.t) (e : Event.t) =
      e.contour_id <- cid; e.other.contour_id <- cid;
      c.contour <- e.pt :: c.contour;
      match V2.equal start.pt e.other.pt with
      | true -> if c.depth mod 2 = 1 then c.contour <- List.rev c.contour
      | false ->
          let another_other = find_kont errs ~panic:start.other evs e.other in
          loop errs evs cid c start another_other
    in
    let cid, c = init_contour errs cs start in
    loop errs evs cid c start start

  let find evs =
    let errs = ref "" in
    let rec loop errs r max i cs =
      if i > max then cs else
      if assigned r.(i) then loop errs r max (i + 1) cs else
      (next_contour errs cs r r.(i); loop errs r max (i + 1) cs)
    in
    let css = loop errs evs (Array.length evs - 1) 0 (make ~size:256) in
    let r = final_polygon css in
    if !errs = "" then Ok r else Error (r, !errs)
end

let bool_op_empty_cases op p0 p1 =
  if not (is_empty p0) && not (is_empty p1) then None else match op with
  | Diff -> Some p0
  | Union | Xor -> if is_empty p0 then Some p1 else Some p0
  | Inter -> Some empty

let bool_op_trivial_cases op p0 p1 =
  (* assert (not (is_empty p0) && not (is_empty p1)); *)
  let p0_min, p0_max = min_max p0 in
  let p1_min, p1_max = min_max p1 in
  let no_overlap =
    (P2.x p0_min > P2.x p1_max) || (P2.x p1_min > P2.x p0_max) ||
    (P2.y p0_min > P2.y p1_max) || (P2.y p1_min > P2.y p0_max)
  in
  if not no_overlap then None else match op with
  | Diff -> Some p0
  | Union | Xor -> Some (v (List.rev_append (List.rev p0.cs) p1.cs))
  | Inter -> Some empty

let bool_op_other_cases op p0 p1 =
  let evs, overlap_err = match Sweep.events op p0 p1 with
  | Ok evs -> evs, false | Error evs -> evs, true
  in
  let evs = Event.filter_and_sort evs in
  match Topology.find evs with
  | Ok _ as r -> r
  | Error (r, msg) ->
      if overlap_err
      then Error (r, `Edge_overlap)
      else Error (r, `Topology_panic msg)

let bool_op op p0 p1 = match bool_op_empty_cases op p0 p1 with
| Some r -> Ok (r, (* FIXME we need to compute the topo info *) [])
| None ->
    match bool_op_trivial_cases op p0 p1 with
    | Some r -> Ok (r, (* FIXME we need to compute the topo info *) [])
    | None -> bool_op_other_cases op p0 p1

type holes = int list list
type bool_op_error =
[ `Edge_overlap
| `Topology_panic of string ]

type bool_op_result = (t * holes, (t * holes) * bool_op_error) result
let union p0 p1 = bool_op Union p0 p1
let diff p0 p1 = bool_op Diff p0 p1
let inter p0 p1 = bool_op Inter p0 p1
let xor p0 p1 = bool_op Xor p0 p1

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
