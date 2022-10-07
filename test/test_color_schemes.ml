(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit

(* Colors *)

let str = Format.sprintf
let log f = Format.printf (f ^^ "@?")
let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

let raises f v = try f v; fail "didn't raise" with _ -> ()
let eqf v v' =
  if v = v' then () else
  fail "%f (%a) = %f (%a)" v Float.pp v v' Float.pp v'

let irange ?(min = 0.) ?(max = 1.) ~dt f =
  let n = truncate (((max -. min) /. dt) +. 1.) in
  let maxi = n - 1 in
  let rec loop i =
    if i < maxi then (f (min +. (float i) *. dt); loop (i + 1)) else f max
  in
  if max = min then f max else
  (f min; loop 1)

let ( >>= ) f x = f x

(* internal function, test passed

let test_msc () =
  let test_edge c =
    irange ~min:0. ~max:1. ~dt:0.01 >>= fun t ->
    let c = c t in
    let _, _, h, _ = V4.to_tuple (Color.to_luva ~lch:true c) in
    let c' = Colors.msc h in
    if not (V4.equal_f (Float.equal_tol ~eps:1.e-9) c c') then
      fail "%a != %a\n" V4.pp c V4.pp c';
  in
  test_edge (fun t -> Color.v t  1. 0. 1.);
  test_edge (fun t -> Color.v t  0. 1. 1.);
  test_edge (fun t -> Color.v 0. t  1. 1.);
  test_edge (fun t -> Color.v 1. t  0. 1.);
  test_edge (fun t -> Color.v 0. 1. t  1.);
  test_edge (fun t -> Color.v 1. 0. t  1.)
*)

let test_color_seq () =
  log "Testing sequential color schemes do not NaN.\n";
  irange ~min:0. ~max:359. ~dt:1. >>= fun h ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun w ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun s ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun b ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun c ->
  let cs =
    Color_scheme.sequential_wijffelaars
      ~w ~s ~b ~c ~h:(Float.rad_of_deg h) ()
  in
  irange ~min:0. ~max:1. ~dt:1. >>= fun t ->
  let color = cs t in
  let urange d = 0. <= d && d <= 1. in
  if V4.for_all urange color then () else
  let cr, cg, cb, ca = V4.to_tuple color in
  fail "not in rgb cube w:%g s:%g b:%g c:%g h:%g t:%g \
        (%.16f %.16f %.16f)"
    w s b c h t cr cg cb

let test_qual () =
  log "Testing qualitative color schemes do not NaN.\n";
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun eps ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun r ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun s ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun b ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun c ->
  let size = 16 in
  let q = Color_scheme.qualitative_wijffelaars ~size () in
  for i = 0 to size - 1 do
    let color = q i in
    let urange d = 0. <= d && d <= 1. in
    if V4.for_all urange color then () else
    let cr, cg, cb, _ = V4.to_tuple color in
    fail "qualitative color not in rgb cube eps:%g r:%g s:%g b:%g c:%g \
          (%.16f %.16f %.16f)"
      eps r s b c cr cg cb
  done

let test () =
  Printexc.record_backtrace true;
  test_color_seq ();
  test_qual ();
  log "All tests succeded.\n"

let () = if not !Sys.interactive then test ()

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
