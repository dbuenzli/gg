(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Brr_canvas
open Brr

(* This set of coordinates was found via
   https://observablehq.com/@mourner/non-robust-arithmetic-as-art *)

let p = P2.v 16.5 16.5
let q = P2.v 18.  18.
let r0 = P2.v 0.5 0.5
let window = 2. ** (-42.)

let render_predicate ~w:iw ~h:ih orient =
  let c = Canvas.create ~w:iw ~h:ih [] in
  let w = float iw and h = float ih in
  let ctx = C2d.get_context c in
  C2d.clear_rect ctx ~x:0. ~y:0. ~w ~h;
  let data = C2d.get_image_data ctx ~x:0 ~y:0 ~w:iw ~h:ih in
  let pixels = C2d.Image_data.data data in
  for y = 0 to ih - 1 do
    for x = 0 to iw - 1 do
      let rx = P2.x r0 +. (float x *. (window /. w)) in
      let ry = P2.y r0 +. (float y *. (window /. h)) in
      let r, g, b =
        (* if x = y then 0x00, 0x00, 0x00 else *)
        match orient p q (P2.v rx ry) with
        | o when o < 0. -> 0x38, 0x6c, 0xb0
        | o when o = 0. -> 0xfd, 0xc0, 0x86
        | o (* when o > 0. *) -> 0xf0, 0x02, 0x7f
      in
      let off = 4 * (y * iw + x) in
      Tarray.set pixels (off    ) r;
      Tarray.set pixels (off + 1) g;
      Tarray.set pixels (off + 2) b;
      Tarray.set pixels (off + 3) 0xFF;
    done;
  done;
  C2d.put_image_data ctx data ~x:0 ~y:0;
  Canvas.to_el c

let main () =
  let h1 = El.h1 [El.txt' "Orientation predicates"] in
  let fast = render_predicate ~w:350 ~h:350 P2.orient_fast in
  let robust = render_predicate ~w:350 ~h:350 P2.orient in
  let html = [h1; fast; El.txt' "   "; robust] in
  El.set_children (Document.body G.document) html

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
