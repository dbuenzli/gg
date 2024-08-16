(*---------------------------------------------------------------------------
   Copyright (c) 2024 The gg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

open Gg
open Gg_kit

let rect_cw = Ring2.of_pts [P2.o; P2.v 3. 0.; P2.v 3. 2.; P2.v 0. 2.]
let sq_cw = Ring2.of_pts [P2.o; P2.v 2. 0.; P2.v 2. 2.; P2.v 0. 2.]

let test_area () =
  Test.test "Ring2.{area,swap_orientation}" @@ fun () ->
  Test.float 0. (Ring2.area Ring2.empty);
  Test.float 4. (Ring2.area sq_cw);
  Test.float (-4.) (Ring2.area @@ Ring2.swap_orientation sq_cw);
  Test.float 6. (Ring2.area rect_cw);
  Test.float ~-.6. (Ring2.area @@ Ring2.swap_orientation rect_cw);
  ()

let main () =
  Test.main @@ fun () ->
  test_area ();
  ()

let () = if !Sys.interactive then () else exit (main ())
