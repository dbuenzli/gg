(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Vg
open Gg
open Gg_kit

(* Render *)

let qual = Color_scheme.qualitative ~a:0.5 `Brewer_accent_8 ()

let frame_box ~src ~dst = (* fits and center *)
  let sy = Box2.h dst /. Box2.h src in
  let sx = Box2.w dst /. Box2.w src in
  let s, d =
    if sx < sy
    then sx, V2.v 0. (0.5 *. (Box2.h dst -. sx *. Box2.h src))
    else sy, V2.v (0.5 *. (Box2.w dst -. sy *. Box2.w src)) 0.
  in
  M3.mul (M3.move2 V2.(Box2.o dst + d)) @@
  M3.mul (M3.scale2 (V2.v s s)) @@
  M3.move2 (V2.neg (Box2.o src))

let v_squares ~size ~gutter imgs = (* Start at P2.o and left-aligned on oy *)
  let rec loop size gutter ypos acc = function
  | [] -> Box2.v P2.o (Size2.v (Size2.w size) ypos), acc
  | (b, i) :: is ->
      let dst = Box2.v (V2.v 0. ypos) size in
      let tr = frame_box ~src:b ~dst in
      let acc = I.blend (I.tr tr i) acc in
      let ypos = ypos +. Size2.h size +. if is = [] then 0. else gutter in
      loop size gutter ypos acc is
  in
  loop size gutter 0. I.void imgs

let h_squares ~size ~gutter imgs = (* Start at P2.o and bottom-aligned on ox *)
  let rec loop size gutter xpos acc = function
  | [] -> Box2.v P2.o (Size2.v xpos (Size2.h size)), acc
  | (b, i) :: is ->
      let dst = Box2.v (V2.v xpos 0.) size in
      let tr = frame_box ~src:b ~dst in
      let acc = I.blend (I.tr tr i) acc in
      let xpos = xpos +. Size2.w size +. if is = [] then 0. else gutter in
      loop size gutter xpos acc is
  in
  loop size gutter 0. I.void imgs

let v_stack ~gutter imgs = (* Start at P2.o and left-aligned on oy *)
  let rec loop gutter maxw pos acc = function
  | [] -> Box2.v P2.o (Size2.v maxw (P2.y pos)), acc
  | (b, i) :: is ->
      let tr = V2.(pos - Box2.o b) in
      let acc = I.blend (I.move tr i) acc in
      let dy = Box2.h b +. if is = [] then 0. else gutter in
      let pos = V2.v (P2.x pos) (P2.y pos +. dy) in
      let maxw = Float.max maxw (Box2.w b) in
      loop gutter maxw pos acc is
  in
  loop gutter Float.min_float P2.o I.void imgs

(* Geometry images *)

let cut_pt ~w pt color =
  let p = P.empty |> P.circle pt (0.5 *. w) in
  let outline = `O { P.o with width = 0.15 *. 0.5 *. w } in
  let full = Color.with_a color 1.0 in
  I.cut p (I.const color) |> I.blend (I.cut ~area:outline p (I.const full))

let contour_path ?(acc = P.empty) c =
  let add pt p = if p == acc then P.sub pt p else P.line pt p in
  P.close (Pgon2.Contour.fold_pts add c acc)

let pgon_path p =
  let add c path = contour_path c ~acc:path in
  Pgon2.fold_contours add p P.empty

let cut_pgon ?(area = `Aeo) ?(o = I.const Color.black) ~w:width p i =
  let p = pgon_path p and outline = `O { P.o with width } in
  I.cut ~area p i |> I.blend (I.cut ~area:outline p o)

let cut_box ~w:width b color =
  let outline = `O { P.o with width } in
  I.cut ~area:outline (P.rect b P.empty) (I.const color)

let cut_seg ~w:width p0 p1 color =
  let area = `O { P.o with width } in
  I.cut ~area (P.empty |> P.sub p0 |> P.line p1) (I.const color)

(* Test images *)

let errs = ref 0
let retract loc doc op = function
| Ok (v, _) -> v
| Error ((v, _), err) ->
    let op = match op with
    | `Union -> "union" | `Inter -> "inter" | `Diff -> "diff" | `Xor -> "xor"
    in
    let err = match err with
    | `Edge_overlap -> "Edge overlap in input"
    | `Topology_panic msg -> msg
    in
    let doc = if doc <> "" then doc ^ "\n" else "" in
    incr errs; Fmt.pr "%s: %s %a\n%s%!" loc op Fmt.lines err doc; v

let test (loc, a, b, _, doc) =
  try
    let box = Box2.union (Pgon2.box a) (Pgon2.box b) in
    let w = 0.005 *. Float.min (Box2.w box) (Box2.h box) in
    let cut_arg col p =
      let o = I.const (Color.with_a col 1.0) and a = I.const col in
      cut_pgon ~w:(2. *. w) ~o p a
    in
    let cut_op ?area p = cut_pgon ~w p (I.const (Color.gray 0.9)) in
    let src = box, (cut_arg (qual 0) a) |> I.blend (cut_arg (qual 1) b) in
    let u = box, cut_op (retract loc doc `Union (Pgon2.union a b)) in
    let i = box, cut_op (retract loc doc `Inter (Pgon2.inter a b)) in
    let d = box, cut_op (retract loc doc `Diff (Pgon2.diff a b)) in
    let x = box, cut_op (retract loc doc `Xor (Pgon2.xor a b)) in
    h_squares ~size:Size2.unit ~gutter:0.25 [src; u; i; d; x]
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    incr errs;
    Fmt.pr "@[<v>%s: raised@,%a@]" loc Fmt.exn_backtrace (exn, bt);
    Box2.v P2.o Size2.zero, I.void

let render r (view, img) =
   let aspect = Size2.aspect (Box2.size view) in
   let size = Size2.of_w 200. (* mm *) ~aspect in
   ignore (Vgr.render r (`Image (size, view, img)));
   ignore (Vgr.render r `End)

let main () =
  let title = "Pgon bool" in
  let description = "Pgon boolean operators tests" in
  let xmp = Vgr.xmp ~title ~description () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let file = "/tmp/pgon-test.pdf" in
  Out_channel.with_open_bin file @@ fun oc ->
  let r = Vgr.create ~warn (Vgr_pdf.target ~xmp ()) (`Channel oc) in
  let result _ m =
    if !errs > 0
    then m (format_of_string "%s tests failed") (string_of_int !errs)
    else m (format_of_string "%sSuccess!") ""
  in
  (Log.time ~level:Log.App result @@ fun () ->
   let img =
     let tests = List.rev_map test Pgon2_test_cases.list in
     let box, i = v_stack ~gutter:0.1 tests in
     Box2.outset (Size2.v 0.5 0.1) box, i
   in
   render r img);
  Printf.printf "Wrote %s\n%!" file;
  exit !errs

let () = main ()


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
