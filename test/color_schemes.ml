(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit
open Vg

(* TODO code a proper Vz color scheme explorator with sliders etc. *)

let log fmt = Format.printf (fmt ^^ "@.")

(* Color schemes to display. *)

let schemes =
  let h = Float.rad_of_deg in
  [ `C (true, Color_scheme.seq ~w:0. ~h:(h 255.) (), 9.);
    `D (true, Color_scheme.seq_d ~w:0. ~h:(h 255.) 9);
    `D (true, Color_scheme.seq_d ~w:0.15 ~h:(h 255.) 9);
    `D (true, Color_scheme.seq_d ~w:0.9 ~h:(h 255.) 9);
    `D (true, Color_scheme.seq_d ~w:0.5 ~h:(h 10.) 9);
    `D (true, Color_scheme.seq_d ~h:(h 10.) 9);
    `D (true, Color_scheme.seq_d ~s:0.5 ~b:1. ~h:(h 255.) 9);
    `Blank;
    `C (true, Color_scheme.turbo (), 9.);
    `Blank;
    `C (true, (Color_scheme.div ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) ()), 9.);
    `D (true, Color_scheme.div_d ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) 9);
    `D (true, Color_scheme.div_d ~m:0.6 ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) 9);
    `C (true,
        (Color_scheme.div ~m:0.6 ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) ()), 9.);
    `D (true, Color_scheme.div_d ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) 8);
    `D (true, Color_scheme.div_d ~w:0. ~h0:(h 255.) ~h1:(h 10.) 9);
    `D (true, Color_scheme.div_d ~w:1. ~s:0.7 ~h0:(h 120.) ~h1:(h 10.) 9);
    `D (true,
        Color_scheme.div_d ~h0:(h 255.) ~h1:(h 10.) ~s:0.3 ~c:0.9 ~b:0.5 6);
    `Blank;
    `D (false, Color_scheme.qual_fixed `Brewer_accent_8);
    `D (false, Color_scheme.qual_fixed `Brewer_dark2_8);
    `D (false, Color_scheme.qual_fixed `Brewer_paired_12);
    `D (false, Color_scheme.qual_fixed `Brewer_pastel1_9);
    `D (false, Color_scheme.qual_fixed `Brewer_pastel2_8);
    `D (false, Color_scheme.qual_fixed `Brewer_set1_9);
    `D (false, Color_scheme.qual_fixed `Brewer_set2_8);
    `D (false, Color_scheme.qual_fixed `Brewer_set3_12);
    `D (false, Color_scheme.qual_fixed `Wijffelaars_17);
    `D (false, Color_scheme.qual_d 8);
    `D (false, Color_scheme.qual_d ~c:0.8 8); ]

(* Continuous and discrete color schemes as images *)

let range ?(min = 0.) ?(max = 1.) dt f acc =
  let n = truncate (((max -. min) /. dt) +. 1.) in
  let maxi = n - 1 in
  let rec loop acc i =
    if i < maxi then loop (f acc (min +. (float i) *. dt)) (i + 1) else
    f acc max
  in
  loop (f acc min) 1

let colors ?(rev = false) cs len =  (* scheme sampled every 0.05 unit of len *)
  let dt = 1. /. (floor ((len /. 0.05) +. 1.)) in
  let add_sample acc t = ((if rev then 1. -. t else t), cs t) :: acc in
  let stops =
    let stops = range dt add_sample [] in
    if rev then stops else List.rev stops
  in
  let bounds = P.empty |> P.rect (Box2.v P2.o (Size2.v 1. len)) in
  I.axial stops P2.o (P2.v 0. len) |> I.cut bounds

let colors_d ?(rev = false) cs =                   (* discrete color scheme. *)
  let sq =
    let sq = Box2.v P2.o (Size2.v 1. 1.01 (* overlap *)) in
    P.empty |> P.rect sq
  in
  let bounds =
    let n = Array.length cs in
    P.empty |> P.rect (Box2.v P2.o (Size2.v 1. (float n)))
  in
  let mv = P2.v 0. 1.0 in
  let add acc c = acc |> I.move mv |> I.blend (I.const c |> I.cut sq) in
  let colors =
    if rev then Array.fold_left add I.void cs else
    Array.fold_right (fun c acc -> add acc c) cs I.void
  in
  colors |> I.cut bounds (* cut topmost overlap *)

let size = Size2.v 200. 100. (* mm *)
let view = Box2.v P2.o (Size2.v 40. 20.)
let image =
  let add scheme acc =
    let i = match scheme with
    | `D (rev, cs) -> colors_d ~rev cs
    | `C (rev, cs, n) -> colors ~rev cs n
    | `Blank -> I.void
    in
    acc |> I.move (P2.v 1.5 0.0) |> I.blend i
  in
  List.fold_right add schemes I.void
  |> I.scale (V2.v 1. (-1.)) |> I.move (V2.v 0. 20.)

(* Browser bureaucracy. *)

open Brr
open Brr_canvas

let main () =
  let body = (Document.body G.document) in
  let c = Canvas.create [] in
  let () = El.append_children body [Canvas.to_el c] in
  let r =
    let t = Vgr_htmlc.target (* ~resize:true *) (Obj.magic c) in
    Vg.Vgr.create t `Other
  in
  assert(Vgr.render r (`Image (size, view, image)) = `Ok);
  ()

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers

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
