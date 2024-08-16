(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Gg tests.

   Given a Gg module M, its tests have names that start with "M."  and
   are defined in a module M_tests. Most of the time a test name
   correspond to the name(s) of tested function, but often other
   functions are also tested implicitely.

   Some tests use exact binary comparison between floats but this is
   only done when the result should be exact w.r.t to IEEE 754's
   properties. *)

open Checkm
open Checkm.C.Special
open Gg

let str = Format.asprintf
let eps = 1e-9

module Test = Checkm.Test                                 (* help ocamlbuild. *)

let ( & ) f x = f x

(* Float tests *)

module Float_tests = struct
  let test n f = Test.add_test ("Float." ^ n) f

  (* Float generators. *)

  let pp_bfloat pp f = Format.fprintf pp "%F@ (%a)" f Float.pp f
  let any_float =
    let g _ rs =
      let r2 = Int64.shift_left (Int64.of_int (Random.State.bits rs)) 34 in
      let r1 = Int64.shift_left (Int64.of_int (Random.State.bits rs)) 4 in
      let r0 = Int64.of_int (Random.State.bits rs land 0xF) in
      Int64.float_of_bits (Int64.logor r2 (Int64.logor r1 r0))
    in
    g, pp_bfloat

  let uint_float =
    let g _ rs = Int64.to_float (Random.State.int64 rs Int64.max_int) in
    g, Format.pp_print_float

  module Testable_float = struct
    include (Float : C.Testable with type t = float)
    let print =  pp_bfloat
  end

  module Cf = C.Make (Testable_float)
  open Cf.Order

  let () = test "max_frac_float" & fun r ->
      r
      >> Cf.holds (C.neg Float.is_integer) Float.max_frac_float
      >> Cf.holds (C.neg Float.is_integer) (-. Float.max_frac_float)
      >> Cf.holds Float.is_integer (Float.max_frac_float +. 1.)
      >> Cf.holds Float.is_integer (-. Float.max_frac_float -. 1.)
      >> C.success

  let () = test "max_int_arith" & fun r ->
      r
      >> (ldexp 1. 53 = Float.max_int_arith)
      >> (Float.max_int_arith <> Float.max_int_arith -. 1.)
      >> Cf.holds Float.is_integer (Float.max_int_arith -. 1.)
      >> (Float.max_int_arith +. 1. = Float.max_int_arith) (* rnd mode dep ? *)
      >> (Float.max_int_arith +. 2. = Float.succ Float.max_int_arith)
      >> C.success

  let () = test "deg_of_rad" & fun r ->
      r
      >> (Float.deg_of_rad 0. = 0.)
      >> (abs_float (Float.deg_of_rad Float.pi -. 180.) < 1e-10)
      >> Cf.holds Float.is_nan (Float.deg_of_rad nan)
      >> C.success

  let () = test "rad_of_deg" & fun r ->
      r
      >> (Float.rad_of_deg 0. = 0.)
      >> (abs_float (Float.rad_of_deg 180. -. Float.pi) < 1e-10)
      >> Cf.holds Float.is_nan (Float.rad_of_deg nan)
      >> C.success

  let () = test "wrap_angle" & fun r ->
    let is_num f = not (Float.is_nan f || Float.is_infinite f) in
    r
    >> (abs_float (Float.wrap_angle 0.) < 1e-10)
    >> (abs_float (Float.wrap_angle (2. *. Float.pi)) < 1e-10)
    >> (abs_float (Float.wrap_angle (4. *. Float.pi)) < 1e-10)
    >> (abs_float (Float.wrap_angle (6. *. Float.pi)) < 1e-10)
    >> (abs_float (Float.wrap_angle (-2. *. Float.pi)) < 1e-10)
    >> (abs_float (Float.wrap_angle (-4. *. Float.pi)) < 1e-10)
    >> (abs_float (Float.wrap_angle (-6. *. Float.pi)) < 1e-10)
    >> (abs_float ((Float.wrap_angle Float.pi) +. Float.pi) < 1e-10)
    >> (abs_float ((Float.wrap_angle (3. *. Float.pi)) +. Float.pi) < 1e-10)
    >> (abs_float ((Float.wrap_angle (5. *. Float.pi)) +. Float.pi) < 1e-10)
    >> (abs_float ((Float.wrap_angle (-.Float.pi)) +. Float.pi) < 1e-10)
    >> (abs_float ((Float.wrap_angle (-.3. *. Float.pi)) +. Float.pi) < 1e-10)
    >> (abs_float ((Float.wrap_angle (-.5. *. Float.pi)) +. Float.pi) < 1e-10)
    >> Cf.for_all ~cond:is_num any_float
      begin fun v r ->
        let w = Float.wrap_angle v in r
        >> (w < Float.pi)
        >> (w >= -.Float.pi)
        >> C.success
      end
    >> Cf.holds Float.is_nan (Float.wrap_angle nan)
    >> C.success

  let () = test "random functions" & fun r ->
      r
      >> C.for_all Gen.unit
        begin fun () r ->
          let s = Random.get_state () in
          let v = Float.random ~min:(-2.) ~len:4. () in
          let v' = Float.srandom ~min:(-2.) ~len:4. s () in
          r >> (v = v') >> (-2. <= v) >> (v <= 2.) >> C.success
        end
      >> C.success

  let () = test "mix" & fun r ->
      r
      >> (Float.mix 1. 3. 0.5 = 2.)
      >> (Float.mix 1. 3. 0. = 1.)
      >> (Float.mix 1. 3. 1. = 3.)
      >> Cf.holds Float.is_nan (Float.mix nan 3. 0.)
      >> Cf.holds Float.is_nan (Float.mix 1. nan 0.)
      >> Cf.holds Float.is_nan (Float.mix 0. 0. nan)
      >> C.success

  let () = test "step" & fun r ->
      r
      >> (Float.step 4. (-3.) = 0.)
      >> (Float.step 4. 3. = 0.)
      >> (Float.step 4. 4. = 1.)
      >> (Float.step 4. 5. = 1.)
      >> C.success

  let () = test "smooth_step" & fun r ->
      r
      >> (Float.smooth_step 2. 4. (-3.) = 0.)
      >> (Float.smooth_step 2. 4. 2. = 0.)
      >> (Float.smooth_step 2. 4. 3. = 0.5)
      >> (Float.smooth_step 2. 4. 4. = 1.)
      >> (Float.smooth_step 2. 4. 5. = 1.)
      >> C.success

  let () = test "max_num" & fun r ->
      r
      >> (Float.max_num 2. 3. = 3.)
      >> (Float.max_num 3. 2. = 3.)
      >> (Float.max_num nan 3. = 3.)
      >> (Float.max_num 3. nan = 3.)
      >> Cf.holds Float.is_nan (Float.max_num nan nan)
      >> C.success

  let () = test "min_num" & fun r ->
      r
      >> (Float.min_num 2. 3. = 2.)
      >> (Float.min_num 3. 2. = 2.)
      >> (Float.min_num nan 2. = 2.)
      >> (Float.min_num 2. nan = 2.)
      >> Cf.holds Float.is_nan (Float.min_num nan nan)
      >> C.success

  let () = test "clamp" & fun r ->
      r
      >> (Float.clamp ~min:1. ~max:3. (-1.) = 1.)
      >> (Float.clamp ~min:1. ~max:3. 1. = 1.)
      >> (Float.clamp ~min:1. ~max:3. 2. = 2.)
      >> (Float.clamp ~min:1. ~max:3. 3. = 3.)
      >> (Float.clamp ~min:1. ~max:3. 4. = 3.)
      >> C.success

  let () = test "remap" & fun r ->
      r
      >> (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:4. 0. = 0.)
      >> (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:4. 2. = 1.)
      >> (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:4. 4. = 2.)
      >> (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:4. 6. = 3.)
      >> (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:4. 8. = 4.)
      >> (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:4. 10. = 5.)
      >> (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:4. 12. = 6.)
      >> (Float.remap ~x0:1. ~x1:1. ~y0:4. ~y1:5. 6. = 4.)
      >> (Float.remap ~x0:1. ~x1:1. ~y0:5. ~y1:4. 6. = 5.)
      >> (Float.remap ~x0:2. ~x1:4. ~y0:4. ~y1:2. 3. = 3.)
      >> (Float.remap ~x0:2. ~x1:4. ~y0:4. ~y1:2. 2. = 4.)
      >> (Float.remap ~x0:2. ~x1:4. ~y0:4. ~y1:2. 4. = 2.)
      >> Cf.holds Float.is_nan (Float.remap ~x0:nan ~x1:8. ~y0:2. ~y1:4. 4.)
      >> Cf.holds Float.is_nan (Float.remap ~x0:4. ~x1:nan ~y0:2. ~y1:4. 4.)
      >> Cf.holds Float.is_nan (Float.remap ~x0:4. ~x1:8. ~y0:nan ~y1:4. 4.)
      >> Cf.holds Float.is_nan (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:nan 4.)
      >> Cf.holds Float.is_nan (Float.remap ~x0:4. ~x1:8. ~y0:2. ~y1:4. nan)
      >> C.success

  let () = test "round" & fun r ->
      r
      >> (Float.round (-2.8) = -3.)
      >> (Float.round (-2.5) = -3.)
      >> (Float.round (-2.2) = -2.)
      >> (Float.round 0. = 0.)
      >> (Float.round 2.2 = 2.)
      >> (Float.round 2.5 = 3.)
      >> (Float.round 2.8 = 3.)
      >> (Float.round neg_infinity = neg_infinity)
      >> (Float.round infinity = infinity)
      >> Cf.holds Float.is_nan (Float.round nan)
      >> C.success

  open Ci.Order
  let () = test "int_of_round" & fun r ->
      r
      >> (Float.int_of_round (-2.8) = -3)
      >> (Float.int_of_round (-2.5) = -3)
      >> (Float.int_of_round (-2.2) = -2)
      >> (Float.int_of_round 0. = 0)
      >> (Float.int_of_round 2.2 = 2)
      >> (Float.int_of_round 2.5 = 3)
      >> (Float.int_of_round 2.8 = 3)
      >> C.success

  open Cf.Order
  let () = test "round_dfrac" & fun r ->
      r
      >> (Float.round_dfrac 1 (-1.28) = -1.3)
      >> (Float.round_dfrac 1 (-1.25) = -1.2)
      >> (Float.round_dfrac 1 (-1.22) = -1.2)
      >> (Float.round_dfrac 3 (-0.0035) = -0.003)
      >> (Float.round_dfrac 2 (-0.0035) = 0.)
      >> (Float.round_dfrac 1 (-0.) = 0.)
      >> (Float.round_dfrac 1 0. = 0.)
      >> (Float.round_dfrac 3 0.0035 = 0.004)
      >> (Float.round_dfrac 1 1.22 = 1.2)
      >> (Float.round_dfrac 1 1.25 = 1.3)
      >> (Float.round_dfrac 1 1.28 = 1.3)
      >> (Float.round_dfrac 4 1.00044 = 1.0004)
      >> (Float.round_dfrac 4 1.000445 = 1.0004)
      >> (Float.round_dfrac 4 1.000449 = 1.0004)
      >> (Float.round_dfrac 4 1.000450 = 1.0005)
      >> (Float.round_dfrac 16 neg_infinity = neg_infinity)
      >> (Float.round_dfrac 16 infinity = infinity)
      >> Cf.holds Float.is_nan (Float.round_dfrac 16 nan)
      >> C.success

  let () = test "round_dsig" & fun r ->
      r
      >> (Float.round_dsig 1 (-1.28e7) = -1.3e7)
      >> (Float.round_dsig 1 (-1.25e5) = -1.2e5)
      >> (Float.round_dsig 1 (-1.22e2) = -1.2e2)
      >> (Float.round_dsig 2 (-0.002e3) = -0.002e3)
      >> (Float.round_dsig 0 (-0.00243e4) = -0.002e4)
      >> (Float.round_dsig 1 (-0.00243e6) = -0.0024e6)
      >> (Float.round_dsig 1 0.00456e7 = 0.0046e7)
      >> (Float.round_dsig 1 1.22e8 = 1.2e8)
      >> (Float.round_dsig 1 1.25e5 = 1.3e5)
      >> (Float.round_dsig 1 1.28e2 = 1.3e2)
      >> (Float.round_dsig 4 1.00044e25 = 1.0004e25)
      >> (Float.round_dsig 4 1.000445e20 = 1.0004e20)
      >> (Float.round_dsig 4 1.000449e10 = 1.0004e10)
      >> (Float.round_dsig 4 1.000450e6 = 1.0005e6)
      >> Cf.holds Float.is_nan (Float.round_dsig 16 neg_infinity)
      >> Cf.holds Float.is_nan (Float.round_dsig 16 infinity)
      >> Cf.holds Float.is_nan (Float.round_dsig 16 nan)
      >> C.success

  let () = test "round_zero" & fun r ->
      let eps = 0.005 in
      r
      >> (Float.round_zero ~eps (-0.0051) = -0.0051)
      >> (Float.round_zero ~eps (-0.0050) = -0.0050)
      >> (Float.round_zero ~eps (-0.0049) = -0.)
      >> (Float.round_zero ~eps 0. = 0.)
      >> (Float.round_zero ~eps 0.0049 = 0.)
      >> (Float.round_zero ~eps 0.0050 = 0.0050)
      >> (Float.round_zero ~eps 0.0051 = 0.0051)
      >> (Float.round_zero ~eps infinity = infinity)
      >> (Float.round_zero ~eps neg_infinity = neg_infinity)
      >> Cf.holds Float.is_nan (Float.round_zero ~eps nan)
      >> C.success

  let () = test "chop" & fun r ->
      let eps = 0.0005 in
      r
      >> (Float.chop ~eps:0.6 2.5 = 3.)
      >> (Float.chop ~eps:0.6 (-2.5) = -2.)
      >> (Float.chop ~eps (-2.00051) = -2.00051)
      >> (Float.chop ~eps (-2.00050) = -2.00050)
      >> (Float.chop ~eps (-2.00049) = -2.)
      >> (Float.chop ~eps (-2.) = -2.)
      >> (Float.chop ~eps 2. = 2.)
      >> (Float.chop ~eps 2.00049 =  2.)
      >> (Float.chop ~eps 2.00050 =  2.00050)
      >> (Float.chop ~eps 2.00051 =  2.00051)
      >> (Float.chop ~eps infinity = infinity)
      >> (Float.chop ~eps neg_infinity = neg_infinity)
      >> Cf.holds Float.is_nan (Float.chop ~eps nan)
      >> C.success

  let () = test "sign" & fun r ->
      r
      >> (Float.sign 0. = 0.)
      >> (Float.sign (-0.) = 0.)
      >> (Float.sign (-3.) = -1.)
      >> (Float.sign (3.) = 1.)
      >> (Float.sign neg_infinity = -1.)
      >> (Float.sign infinity = 1.)
      >> Cf.holds Float.is_nan (Float.sign nan)
      >> C.success

  let () = test "sign_bit" & fun r ->
      r
      >> Cf.holds Float.sign_bit (-. nan)
      >> Cf.holds Float.sign_bit neg_infinity
      >> Cf.holds Float.sign_bit (-3.)
      >> Cf.holds Float.sign_bit (-0.)
      >> Cf.holds (C.neg Float.sign_bit) 0.
      >> Cf.holds (C.neg Float.sign_bit) 3.
      >> Cf.holds (C.neg Float.sign_bit) infinity
      >> Cf.holds (C.neg Float.sign_bit) nan
      >> C.success

  let () = test "succ" & fun r ->
      r
      >> Cf.holds Float.is_nan (Float.succ (-. nan))
      >> (Float.succ neg_infinity = -. max_float)
      >> (Float.succ (-. Float.min_sub_float) = -0.)
      >> (Float.succ (-0.) = Float.min_sub_float)
      >> (Float.succ (0.) = Float.min_sub_float)
      >> (Float.succ (1.) = 1. +. epsilon_float)
      >> (Float.succ max_float = infinity)
      >> (Float.succ infinity = infinity)
      >> Cf.holds Float.is_nan (Float.succ nan)
      >> C.success

  let () = test "pred" & fun r ->
      r
      >> Cf.holds Float.is_nan (Float.pred nan)
      >> (Float.pred infinity = max_float)
      >> (Float.pred (1. +. epsilon_float) = 1.)
      >> (Float.pred (Float.min_sub_float) = 0.)
      >> (Float.pred (0.) = -. Float.min_sub_float)
      >> (Float.pred (-. 0.) = -. Float.min_sub_float)
      >> (Float.pred (-. max_float) = neg_infinity)
      >> (Float.pred neg_infinity = neg_infinity)
      >> Cf.holds Float.is_nan (Float.pred (-. nan))
      >> C.success

  let magic_payload = 0x43616D6C

  open Ci.Order
  let () = test "nan_nan_payload" & fun r ->
    let n = Float.nan_with_payload magic_payload in
    r
    >> Cf.holds Float.is_nan n
    >> (Float.nan_payload n = magic_payload)
    >> (Float.nan_payload (Stdlib.nan) = 0x1)
    >> Cf.raises_invalid_arg Float.nan_payload 567.
    >> Cf.raises_invalid_arg Float.nan_payload max_float
    >> Cf.raises_invalid_arg Float.nan_payload neg_infinity
    >> C.success

  open Cf.Order

  let () = test "is_zero" & fun r ->
      r
      >> Cf.holds (C.neg (Float.is_zero ~eps)) infinity
      >> Cf.holds (C.neg (Float.is_zero ~eps)) Stdlib.nan
      >> Cf.holds (C.neg (Float.is_zero ~eps))
        (Float.nan_with_payload magic_payload)
      >> Cf.holds (C.neg (Float.is_zero ~eps)) 1e-8
      >> Cf.holds (C.neg (Float.is_zero ~eps)) 1e-9
      >> Cf.holds (Float.is_zero ~eps) 1e-10
      >> Cf.holds (Float.is_zero ~eps) 1e-11
      >> C.success

  let () = test "is_infinite" & fun r ->
      r
      >> Cf.holds Float.is_infinite infinity
      >> Cf.holds Float.is_infinite neg_infinity
      >> Cf.holds (C.neg Float.is_infinite) Stdlib.nan
      >> Cf.holds (C.neg Float.is_infinite)
        (Float.nan_with_payload magic_payload)
      >> Cf.holds (C.neg Float.is_infinite) 0.
      >> Cf.holds (C.neg Float.is_infinite) 3.
      >> C.success

  let () = test "is_integer" & fun r ->
      r
      >> Cf.holds (C.neg Float.is_integer) nan
      >> Cf.holds (C.neg Float.is_integer)
        (Float.nan_with_payload magic_payload)
      >> Cf.holds (C.neg Float.is_integer) infinity
      >> Cf.holds (C.neg Float.is_integer) neg_infinity
      >> Cf.holds (C.neg Float.is_integer) Float.max_sub_float
      >> Cf.holds (C.neg Float.is_integer) Float.min_sub_float
      >> Cf.holds (C.neg Float.is_integer) Float.max_frac_float
      >> Cf.holds Float.is_integer max_float
      >> Cf.holds Float.is_integer Float.max_int_arith
      >> Cf.holds Float.is_integer 0.
      >> Cf.holds Float.is_integer (-0.)
      >> Cf.holds Float.is_integer (-. max_float)
      >> Cf.holds Float.is_integer (-. Float.max_int_arith)
      >> Cf.for_all uint_float begin fun x r ->
        let frac_neighbours x r =
          if Stdlib.(>) x Float.max_frac_float then r else
          r >> Cf.holds (C.neg Float.is_integer) (Float.pred x)
          >> Cf.holds (C.neg Float.is_integer) (Float.succ x)
          >> C.success
        in
        r >> C.holds Float.is_integer x
        >> Cf.holds Float.is_integer (-. x)
        >> frac_neighbours x
        >> C.success
      end
      >> C.success

  let () = test "equal_tol" & fun r ->
      let eps = 0.001 in
      r
      >> Cf.holds (C.neg (Float.equal_tol ~eps 499.4)) 500.
      >> Cf.holds (C.neg (Float.equal_tol ~eps 500.)) 499.4
      >> Cf.holds (Float.equal_tol ~eps 500.) 499.5
      >> Cf.holds (Float.equal_tol ~eps 500.) 499.6
      >> Cf.holds (Float.equal_tol ~eps 500.) 500.0

      >> Cf.holds (Float.equal_tol ~eps 0.) 0.
      >> Cf.holds (C.neg (Float.equal_tol ~eps 0.)) 3.
      >> Cf.holds (C.neg (Float.equal_tol ~eps 0.)) infinity
      >> Cf.holds (C.neg (Float.equal_tol ~eps 0.)) neg_infinity
      >> Cf.holds (C.neg (Float.equal_tol ~eps 0.)) nan

      >> Cf.holds (C.neg (Float.equal_tol ~eps 3.)) 0.
      >> Cf.holds (Float.equal_tol ~eps 3.) 3.
      >> Cf.holds (C.neg (Float.equal_tol ~eps 3.)) infinity
      >> Cf.holds (C.neg (Float.equal_tol ~eps 3.)) neg_infinity
      >> Cf.holds (C.neg (Float.equal_tol ~eps 3.)) nan

      >> Cf.holds (C.neg (Float.equal_tol ~eps infinity)) 0.
      >> Cf.holds (C.neg (Float.equal_tol ~eps infinity)) 3.
      >> Cf.holds (Float.equal_tol ~eps infinity) infinity
      >> Cf.holds (C.neg (Float.equal_tol ~eps infinity)) neg_infinity
      >> Cf.holds (C.neg (Float.equal_tol ~eps infinity)) nan

      >> Cf.holds (C.neg (Float.equal_tol ~eps neg_infinity)) 0.
      >> Cf.holds (C.neg (Float.equal_tol ~eps neg_infinity)) 3.
      >> Cf.holds (C.neg (Float.equal_tol ~eps neg_infinity)) infinity
      >> Cf.holds (Float.equal_tol ~eps neg_infinity) neg_infinity
      >> Cf.holds (C.neg (Float.equal_tol ~eps neg_infinity)) nan

      >> Cf.holds (C.neg (Float.equal_tol ~eps nan)) 0.
      >> Cf.holds (C.neg (Float.equal_tol ~eps nan)) 3.
      >> Cf.holds (C.neg (Float.equal_tol ~eps nan)) infinity
      >> Cf.holds (C.neg (Float.equal_tol ~eps nan)) neg_infinity
      >> Cf.holds (Float.equal_tol ~eps nan) nan

      >> Cf.for_all any_float begin fun x r -> r
        >> Cf.holds (Float.equal_tol ~eps x) x
        >> C.success
      end
      >> C.success

  let float_trip x r =
    let pr ppf x = Float.pp ppf (Int64.float_of_bits x) in
    let x' = float_of_string (str "%a" Float.pp x) in
    r
    >> C.Order.(=) ~pr (Int64.bits_of_float x) (Int64.bits_of_float x')
    >> C.success

  let () = test "pp, non nan" & fun r ->
      r
      >> float_trip (0. /. -1.)
      >> float_trip 0.
      >> float_trip infinity
      >> float_trip neg_infinity
      >> float_trip Float.min_sub_float
      >> float_trip (-. Float.min_sub_float)
      >> float_trip Float.max_sub_float
      >> float_trip (-. Float.max_sub_float)
      >> float_trip max_float
      >> float_trip (-. max_float)
      >> Cf.for_all ~cond:(C.neg Float.is_nan) any_float float_trip
      >> C.success

(*
  let () = test "to_string, any" & fun r ->
    let skip_msg =
      "Negative NaNs are not well handled by strtod on this platform. \
  float_of_string (Float.to_string n) with a negative NaN will \
      not round trip."
    in
    r >> C.catch (float_trip (-. nan)) Test.skip skip_msg
      >> Cf.for_all any_float float_trip
      >> C.success *)
end

(* Vector tests *)

module type V  = sig
  include Gg.V
  val gen : min:float -> len:float -> t Checkm.gen         (* random vector. *)
end

module V_tests (V : V) = struct                            (* generic tests. *)
  let v_prefix = "V" ^ (string_of_int V.dim) ^ "."
  let test n f = Test.add_test (v_prefix ^ n) f
  let g_v = V.gen ~min:(-100.) ~len:200.
  let indices =                                (* list of component indexes. *)
    let o = ref [] in
    for i = V.dim - 1 downto 0 do o := i :: !o done;
    !o

  (* Test vector, relies on correct V.map and V.mapi. The value at
     index_i is i. Useful for tests as each component is different in a
     predictable way. *)

  let index = V.mapi (fun i _ -> float i) V.zero
  let db_index = V.map (fun x -> 2. *. x) index
  let sq_index = V.map (fun x -> x *. x) index

  let ones = V.map (fun _ -> 1.) V.zero

  module Cv = C.Make (V)

  (* Constructors, accessors and constants *)

  open Cf.Order
  let () = test "comp" & fun r ->
    let check r i = r >> (V.comp i index = float i) in
    List.fold_left check r indices >> C.success

  (* Functions *)

  open Cv.Order
  let () = test "neg" & fun r ->
      r
      >> Cv.for_all g_v
        begin fun v r -> r
          >> (V.neg (V.neg v) = v)
          >> (V.neg v = V.smul (-1.) v)
          >> C.success
        end
      >> C.success

  let () = test "add" & fun r ->
      r >> Cv.for_all g_v
        begin fun v r -> r
          >> (V.add (V.neg v) v = V.zero)
          >> (V.add v (V.neg v) = V.zero)
          >> C.success
        end
      >> C.success

  let () = test "sub" & fun r ->
      r
      >> Cv.for_all g_v
        begin fun v r -> r
          >> (V.sub v v = V.zero)
          >> (V.sub (V.neg v) (V.neg v) = V.zero)
          >> C.success
        end
      >> C.success

  let () = test "mul" & fun r ->
      r
      >> (V.mul index index = sq_index)
      >> C.success

  let () = test "div" & fun r ->
    let inc_index = V.map (fun x -> x +. 1.) index in (* to avoid /. zero *)
    r
    >> (V.div (V.mul inc_index inc_index) inc_index = inc_index)
    >> C.success

  let () = test "smul" & fun r ->
    r
    >> (V.smul 2. index = db_index)
    >> (V.smul (-2.) index = V.neg db_index)
    >> C.success

  let () = test "half" & fun r ->
    r
    >> (V.half db_index = index) >> C.success

  open Cf.Order
  let () = test "dot, norm, norm2" & fun r ->
    let sq_sum n = (n * (n + 1) * (2 * n + 1)) / 6 in
    r
    >> (V.dot index index = V.norm2 index)
    >> (V.norm2 index = float (sq_sum (V.dim - 1)))
    >> (Ci.Order.(Float.compare_tol ~eps (V.norm index)
                    (sqrt (V.norm2 index)) = 0))
    >> (V.norm V.zero = 0.)
    >> C.success

  let () = test "norm {over,under}flow" & fun r ->
      r
      >> (V.norm (V.smul 1e200 ones) = sqrt (float V.dim) *. 1e200)
      >> (V.norm (V.smul 1e-200 ones) = sqrt (float V.dim) *. 1e-200)
      >> C.success

  let () = test "unit" & fun r ->
    let unitable v = Stdlib.(<>) (V.norm v) 0. in
    r
    >> Cv.for_all ~cond:unitable g_v
      begin fun v r -> r
        >> (Float.chop ~eps (V.norm (V.unit v)) = 1.)
        >> C.success
      end
    >> C.success

  let () = test "homogene" & fun r ->
      let h = V.homogene index in
      let imax = V.dim - 1 in
      let check r i =
        if Stdlib.(=) i imax then r >> (V.comp i h = 1.) >> C.success else
        r >> (V.comp i h = (V.comp i index) /. (V.comp imax index)) >> C.success
      in
      List.fold_left check r indices >> C.success

  open Cv.Order
  let () = test "mix" & fun r ->
      let tr_index = V.map (fun x -> 3. *. x) index in
      r
      >> (V.mix index tr_index 0. = index)
      >> (V.mix index tr_index 0.5 = db_index)
      >> (V.mix index tr_index 1.0 = tr_index)
      >> C.success

  (* Traversal *)

  open Cf.Order
  let () = test "map" & fun r ->
      let check r i = r >> (V.comp i db_index = (float i) *. 2.) >> C.success in
      List.fold_left check r indices >> C.success

  let () = test "mapi" & fun r ->
      let check r i = r >> (V.comp i index = (float i)) >> C.success in
      List.fold_left check r indices >> C.success

  open Cv.Order
  let () = test "fold" & fun r ->
      let ro =  V.fold (fun acc c -> int_of_float c :: acc) [] index in
      r
      >> C.Order.(=) (List.rev ro) (indices)
      >> C.success

  let () = test "foldi" & fun r ->
      let ro = V.foldi (fun acc i c -> (i * int_of_float c) :: acc) [] index in
      r
      >> C.Order.(=) ro (List.rev_map (fun x -> x * x) indices)
      >> C.success

  let () = test "iter" & fun r ->
      let acc = ref [] in
      V.iter (fun c -> acc := int_of_float c :: !acc) index;
      r
      >> C.Order.(=) (List.rev !acc) indices
      >> C.success

  let () = test "iteri" & fun r ->
      let acc = ref [] in
      V.iteri (fun i c -> acc := i * int_of_float c :: !acc) index;
      r
      >> C.Order.(=) !acc (List.rev_map (fun x -> x * x) indices)
      >> C.success

  (* Predicates and comparisons *)

  let () = test "for_all" & fun r ->
    let eq = Stdlib.(=) in
    r
    >> Cv.holds (V.for_all (fun x -> eq x 0.)) V.zero
    >> Cv.holds (V.for_all (fun x -> eq x infinity)) V.infinity
    >> Cv.holds (V.for_all (fun x -> eq x neg_infinity)) V.neg_infinity
    >> C.success

  let () = test "exists" & fun r ->
      let check r i =
      let p v = (V.exists (fun c -> Stdlib.(=) c (float i))) v in
      r >> Cv.holds p index >> C.success
      in
      List.fold_left check r indices >> C.success

  open Cb.Order
  let () = test "equal_f" & fun r ->
      r
      >> C.for_all (Gen.t2 g_v g_v)
        begin fun (v, v') r -> r
          >> (V.equal v v = V.equal_f Stdlib.(=) v v)
          >> (V.equal v v' = V.equal_f Stdlib.(=) v v')
          >> C.success
        end
      >> C.success

  open Ci.Order
  let () = test "compare_f" & fun r ->
      r
      >> C.for_all (Gen.t2 g_v g_v)
        begin fun (v, v') r -> r
          >> (V.compare v v = V.compare_f Stdlib.compare v v)
          >> (V.compare v v' = V.compare_f Stdlib.compare v v')
          >> C.success
        end
      >> C.success
end

module V2_tests = struct

  module V2 = struct
    include V2
    let gen ~min ~len =
      let g _ rs =
        let r = Float.srandom rs ~min ~len in
        V2.v (r ()) (r ())
      in
      g, pp
  end

  include V_tests (V2)

  open Cf.Order
  let () = test "x, y" & fun r ->
      r
      >> (V2.x index = 0.)
      >> (V2.y index = 1.)
      >> C.success

  let () = test "angle" & fun r ->
      let chop v = Float.chop ~eps v in
      r
      >> (chop (V2.angle V2.ox) = 0.)
      >> (chop (V2.angle V2.oy) = Float.pi_div_2)
      >> (chop (V2.angle (V2.v (- 1.) 0.)) = Float.pi)
      >> (chop (V2.angle (V2.neg V2.ox)) = (-. Float.pi))
      >> (chop (V2.angle (V2.smul 2. (V2.neg V2.oy))) = -. Float.pi_div_2)
      >> C.success

  open Cv.Order
  let () = test "ox, oy, basis" & fun r ->
      r
      >> (V2.basis 0 = V2.ox)
      >> (V2.basis 1 = V2.oy)
      >> C.success

  let () = test "of_tuple, to_tuple" & fun r ->
      r
      >> (V2.of_tuple (0., 1.) = index)
      >> (V2.of_tuple (V2.to_tuple index) = index)
      >> C.success

  let () = test "of_polar" & fun r ->
      let chop v = V2.map (Float.chop ~eps) v in
      r
      >> (chop (V2.of_polar (V2.v 1. 0.)) = V2.ox)
      >> (chop (V2.of_polar (V2.v 1. Float.pi_div_2)) = V2.oy)
      >> (chop (V2.of_polar (V2.v 1. Float.pi)) = (V2.neg V2.ox))
      >> (chop (V2.of_polar (V2.v 2. (-. Float.pi_div_2))) =
          (V2.smul 2. (V2.neg V2.oy)))
      >> C.success

  let () = test "to_polar" & fun r ->
      let chop v = V2.map (Float.chop ~eps) v in
      r
      >> (chop (V2.to_polar V2.ox) = (V2.v 1. 0.))
      >> (chop (V2.to_polar V2.oy) = (V2.v 1. Float.pi_div_2))
      >> (chop (V2.to_polar (V2.v (- 1.) 0.)) = (V2.v 1. Float.pi))
      >> (chop (V2.to_polar (V2.neg V2.ox)) = (V2.v 1. (-. Float.pi)))
      >> (chop (V2.to_polar (V2.smul 2. (V2.neg V2.oy))) =
          (V2.v 2. (-. Float.pi_div_2)))
      >> C.success

  let () = test "of_v3, of_v4" & fun r ->
      r
      >> (V2.of_v3 (V3.v 0. 1. 2.) = index)
      >> (V2.of_v4 (V4.v 0. 1. 2. 3.) = index)
      >> C.success

  let () = test "polar" & fun r ->
      let chop v = V2.map (Float.chop ~eps) v in
      r
      >> (chop (V2.polar 1. 0.) = V2.ox)
      >> (chop (V2.polar 1. Float.pi_div_2) = V2.oy)
      >> (chop (V2.polar 1. Float.pi) = (V2.neg V2.ox))
      >> (chop (V2.polar 2. (-. Float.pi_div_2)) = (V2.smul 2. (V2.neg V2.oy)))
      >> C.success

  let () = test "ortho" & fun r ->
      r
      >> (V2.ortho V2.ox = V2.oy)
      >> (V2.ortho V2.oy = (V2.neg V2.ox))
      >> (V2.ortho (V2.neg V2.ox) = (V2.neg V2.oy))
      >> (V2.ortho (V2.neg V2.oy) = V2.ox)
      >> C.success

  let () = test "ltr" & fun r ->
      let m = M2.v
          1. 2.
          3. 4.
      in
      r
      >> (V2.ltr m (V2.v 1. 2.) = V2.v 5. 11.)
      >> C.success

  let () = test "tr" & fun r ->
      let m = M3.v
          1. 2. 3.
          4. 5. 6.
          7. 8. 9.
      in
      r
      >> (V2.tr m (V2.v 1. 2.) = V2.v 5. 14.)
      >> C.success
end

module V3_tests = struct

  module V3 = struct
    include V3
    let gen ~min ~len =
      let g _ rs =
        let r = Float.srandom rs ~min ~len in
        V3.v (r ()) (r ()) (r ())
      in
      g, pp
  end

  include V_tests (V3)

  open Cf.Order
  let () = test "x, y, z" & fun r ->
      r
      >> (V3.x index = 0.)
      >> (V3.y index = 1.)
      >> (V3.z index = 2.)
      >> C.success

  let () = test "azimuth" & fun r ->
    let chop v = Float.chop ~eps v in
    r
    >> (chop 0. = V3.azimuth V3.oz)
    >> (chop (-. Float.pi) = V3.azimuth (V3.neg V3.oz))
    >> (chop 0. = V3.azimuth V3.ox)
    >> (chop (-. Float.pi) = V3.azimuth (V3.neg V3.ox))
    >> (chop Float.pi_div_2 = V3.azimuth V3.oy)
    >> (chop (-. Float.pi_div_2) = V3.azimuth (V3.neg V3.oy))
    >> C.success

  let () = test "zenith" & fun r ->
    let chop v = Float.chop ~eps v in
    r
    >> (chop 0. = V3.zenith V3.oz)
    >> (chop Float.pi_div_2 = V3.zenith V3.ox)
    >> (chop Float.pi = V3.zenith (V3.neg V3.oz))
    >> (chop Float.pi_div_2 = V3.zenith V3.oy)
    >> (chop 0. = V3.zenith (V3.smul 2. V3.oz))
    >> (chop Float.pi_div_2 = V3.zenith (V3.neg V3.ox))
    >> (chop Float.pi_div_2 = V3.zenith (V3.neg V3.oy))
    >> C.success

  open Cv.Order
  let () = test "ox, oy, oz, basis" & fun r ->
      r
      >> (V3.basis 0 = V3.ox)
      >> (V3.basis 1 = V3.oy)
      >> (V3.basis 2 = V3.oz)
      >> C.success

  let () = test "of_tuple, to_tuple" & fun r ->
      r
      >> (V3.of_tuple (0., 1., 2.) = index)
      >> (V3.of_tuple (V3.to_tuple index) = index)
      >> C.success

  let () = test "of_spherical" & fun r ->
      let chop v = V3.map (Float.chop ~eps) v in
      r
      >> (chop (V3.of_spherical (V3.v 1. 0. 0.)) = V3.oz)
      >> (chop (V3.of_spherical (V3.v 1. 0. Float.pi_div_2)) = V3.ox)
      >> (chop (V3.of_spherical (V3.v 1. 0. Float.pi)) = V3.neg V3.oz)
      >> (chop (V3.of_spherical (V3.v 1. Float.pi_div_2 0.)) = V3.oz)
      >> (chop (V3.of_spherical (V3.v 1. Float.pi_div_2 Float.pi_div_2)) =
          V3.oy)
      >> (chop (V3.of_spherical (V3.v 1. Float.pi_div_2 Float.pi)) =
          V3.neg V3.oz)
      >> (chop (V3.of_spherical (V3.v 2. Float.pi 0.)) = (V3.smul 2. V3.oz))
      >> (chop (V3.of_spherical (V3.v 1. Float.pi Float.pi_div_2)) =
          V3.neg V3.ox)
      >> (chop (V3.of_spherical (V3.v 1. Float.pi Float.pi)) = V3.neg V3.oz)
      >> (chop (V3.of_spherical (V3.v 1. (-. Float.pi_div_2) 0.)) = V3.oz)
      >> (chop (V3.of_spherical (V3.v 1. (-. Float.pi_div_2) Float.pi_div_2)) =
          (V3.neg V3.oy))
      >> (chop (V3.of_spherical (V3.v 1. (-. Float.pi_div_2) Float.pi)) =
          V3.neg V3.oz)
      >> C.success

  let () = test "to_spherical" & fun r ->
      let chop v = V3.map (Float.chop ~eps) v in
      r
      >> (chop (V3.v 1. 0. 0.) = V3.to_spherical V3.oz)
      >> (chop (V3.v 1. 0. Float.pi_div_2) = V3.to_spherical V3.ox)
      >> (chop (V3.v 1. (-. Float.pi) Float.pi) =
          V3.to_spherical (V3.neg V3.oz))
      >> (chop (V3.v 1. Float.pi_div_2 Float.pi_div_2) = V3.to_spherical V3.oy)
      >> (chop (V3.v 1. (-. Float.pi) Float.pi) =
          V3.to_spherical (V3.neg V3.oz))
      >> (chop (V3.v 2. 0. 0.) = V3.to_spherical (V3.smul 2. V3.oz))
      >> (chop (V3.v 1. (-. Float.pi) Float.pi_div_2) =
          V3.to_spherical (V3.neg V3.ox))
      >> (chop (V3.v 1. (-. Float.pi_div_2) Float.pi_div_2) =
          V3.to_spherical (V3.neg V3.oy))
      >> C.success

  let () = test "of_v2, of_v4" & fun r ->
      r
      >> (V3.of_v2 (V2.v 0. 1.) ~z:2. = index)
      >> (V3.of_v4 (V4.v 0. 1. 2. 3.) = index)
      >> C.success

  let () = test "cross" & fun r ->
      r
      >> (V3.cross V3.ox V3.oy = V3.oz)
      >> (V3.cross V3.oy V3.ox = V3.neg V3.oz)
      >> (V3.cross (V3.neg V3.ox) V3.oy = V3.neg V3.oz)
      >> C.success

  let () = test "spherical" & fun r ->
      let chop v = V3.map (Float.chop ~eps) v in
      r
      >> (chop (V3.spherical 1. 0. 0.) = V3.oz)
      >> (chop (V3.spherical 1. 0. Float.pi_div_2) = V3.ox)
      >> (chop (V3.spherical 1. 0. Float.pi) = V3.neg V3.oz)
      >> (chop (V3.spherical 1. Float.pi_div_2 0.) = V3.oz)
      >> (chop (V3.spherical 1. Float.pi_div_2 Float.pi_div_2) = V3.oy)
      >> (chop (V3.spherical 1. Float.pi_div_2 Float.pi) = V3.neg V3.oz)
      >> (chop (V3.spherical 2. Float.pi 0.) = (V3.smul 2. V3.oz))
      >> (chop (V3.spherical 1. Float.pi Float.pi_div_2) = V3.neg V3.ox)
      >> (chop (V3.spherical 1. Float.pi Float.pi) = V3.neg V3.oz)
      >> (chop (V3.spherical 1. (-. Float.pi_div_2) 0.) = V3.oz)
      >> (chop (V3.spherical 1. (-. Float.pi_div_2) Float.pi_div_2) =
          (V3.neg V3.oy))
      >> (chop (V3.spherical 1. (-. Float.pi_div_2) Float.pi) = V3.neg V3.oz)
      >> C.success

  let () = test "ltr" & fun r ->
      let m = M3.v
          1. 2. 3.
          4. 5. 6.
          7. 8. 9.
      in
      r
      >> (V3.ltr m (V3.v 1. 2. 3.) = V3.v 14. 32. 50.)
      >> C.success

  let () = test "tr" & fun r ->
      let m = M4.v
          1.  2.  3.  4.
          5.  6.  7.  8.
          9.  10. 11. 12.
          13. 14. 15. 16.
      in
      r
      >> (V3.tr m (V3.v 1. 2. 3.) = V3.v 14. 38. 62.)
      >> C.success
end

module V4_tests = struct

  module V4 = struct
    include V4
    let gen ~min ~len =
      let g _ rs =
        let r = Float.srandom rs ~min ~len in
        V4.v (r ()) (r ()) (r ()) (r ())
      in
      g, pp
  end

  include V_tests (V4)

  open Cf.Order
  let () = test "x, y, z, w" & fun r ->
      r
      >> (V4.x index = 0.)
      >> (V4.y index = 1.)
      >> (V4.z index = 2.)
      >> (V4.w index = 3.)
      >> C.success

  open Cv.Order
  let () = test "ox, oy, oz, basis" & fun r ->
      r
      >> (V4.basis 0 = V4.ox)
      >> (V4.basis 1 = V4.oy)
      >> (V4.basis 2 = V4.oz)
      >> (V4.basis 3 = V4.ow)
      >> C.success

  let () = test "of_tuple, to_tuple" & fun r ->
      r
      >> (V4.of_tuple (0., 1., 2., 3.) = index)
      >> (V4.of_tuple (V4.to_tuple index) = index)
      >> C.success

  let () = test "of_v2, of_v3" & fun r ->
      r
      >> (V4.of_v2 (V2.v 0. 1.) ~z:2. ~w:3. = index)
      >> (V4.of_v3 (V3.v 0. 1. 2.) ~w:3. = index)
      >> C.success

  let () = test "ltr" & fun r ->
    let m = M4.v
        1.  2.  3.  4.
        5.  6.  7.  8.
        9.  10. 11. 12.
        13. 14. 15. 16.
    in
    r
    >> (V4.ltr m (V4.v 1. 2. 3. 4.) = V4.v 30. 70. 110. 150.)
    >> C.success
end

(* Point tests *)

module type P  = sig
  include Gg.P
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val gen : min:float -> len:float -> t Checkm.gen           (* random point. *)
end

module P_tests (P : P) (V : Gg.V with type t = P.t) = struct (* generic tests.*)
  let p_prefix = "P" ^ (string_of_int P.dim) ^ "."
  let test n f = Test.add_test (p_prefix ^ n) f
  let g_p = P.gen ~min:(-100.) ~len:200.
  let index = V.mapi (fun i _ -> (float i)) P.o
  let inc_index = V.map (fun x -> x +. 1.) index

  module Cp = C.Make (P)
  open Cp.Order

  let () = test "o" & fun r ->
      r >> Cp.holds (V.for_all (fun c -> Stdlib.(=) c 0.)) P.o >> C.success

  let () = test "mid" & fun r ->
      r
      >> (P.mid inc_index (V.smul 3. inc_index) = V.smul 2. inc_index)
      >> C.success
end

module P2_tests = struct

  module P2 = struct
    include P2
    let compare = V2.compare
    let pp = V2.pp
    let gen = V2_tests.V2.gen
  end

  include P_tests (P2) (V2)

  open Cf.Order
  let () = test "x, y" & fun r ->
      r
      >> (P2.x index = 0.)
      >> (P2.y index = 1.)
      >> C.success

  open Cp.Order
  let () = test "tr" & fun r ->
      let m = M3.v
          1. 2. 3.
          4. 5. 6.
          7. 8. 9.
      in
      r
      >> (P2.tr m (P2.v 1. 2.) = P2.v 8. 20.)
      >> C.success
end

module P3_tests = struct

  module P3 = struct
    include P3
    let compare = V3.compare
    let pp = V3.pp
    let gen = V3_tests.V3.gen
  end

  include P_tests (P3) (V3)

  open Cf.Order
  let () = test "x, y, z" & fun r ->
      r
      >> (P3.x index = 0.)
      >> (P3.y index = 1.)
      >> (P3.z index = 2.)
      >> C.success

  open Cp.Order
  let () = test "tr" & fun r ->
      let m = M4.v
          1.  2.  3.  4.
          5.  6.  7.  8.
          9.  10. 11. 12.
          13. 14. 15. 16.
      in
      r
      >> (P3.tr m (P3.v 1. 2. 3.) = P3.v 18. 46. 74.)
      >> C.success
end

(* Matrix tests *)

module type M = sig
  include Gg.M
  val gen : min:float -> len:float -> t Checkm.gen         (* random matrix. *)
end

module M_tests (M : M) = struct                            (* generic tests. *)
  let m_prefix = "M" ^ (string_of_int M.dim) ^ "."
  let test n f = Test.add_test (m_prefix ^ n) f
  let g_m = M.gen ~min:(-100.) ~len:200.
  let indices = (* list of (row, column, linear index) in column-major order. *)
    let o = ref [] in
    for j = M.dim - 1 downto 0 do
      for i = M.dim - 1 downto 0 do
        o := (i, j, M.dim * j + i) :: !o
      done;
    done;
    !o

  (* Test matrices, relies on correct M.map and M.mapi. The value at
     lindex_ij is the linear column-major index of position
     (i,j). Useful for tests as each element is different in a
     predictable way. *)

  let lindex = M.mapi (fun i j _ -> (float (M.dim * j + i))) M.zero
  let db_lindex = M.map (fun x -> 2. *. x) lindex

  module Cm = C.Make (M)

  (* Functions *)

  open Cm.Order
  let () = test "neg" & fun r ->
      r
      >> Cm.for_all g_m
        begin fun m r -> r
          >> (M.neg (M.neg m) = m)
          >> (M.neg m = M.smul (-1.) m)
          >> C.success
        end
      >> C.success

  let () = test "add" & fun r ->
      r
      >> Cm.for_all g_m
        begin fun m r -> r
          >> (M.add (M.neg m) m = M.zero)
          >> (M.add m (M.neg m) = M.zero)
          >> C.success
        end
      >> C.success

  let () = test "sub" & fun r ->
    r
    >> Cm.for_all g_m
      begin fun m r -> r
        >> (M.sub m m = M.zero)
        >> (M.sub (M.neg m) (M.neg m) = M.zero)
        >> C.success
      end
    >> C.success

  let () = test "mul" & fun r ->        (* M.mul is better tested by M4.inv *)
      r
      >> Cm.for_all g_m
        begin fun m r ->
          r >> (M.mul M.id m = m)
          >> (M.mul m M.id = m)
          >> C.success
        end
      >> C.success

  let () = test "emul" & fun r ->
      r
      >> (M.emul lindex lindex = M.map (fun x -> x *. x) lindex)
      >> C.success

  let () = test "ediv" & fun r ->
      let inc_lindex = M.map (fun x -> x +. 1.) lindex in (* to avoid /. zero *)
      r
      >> (M.ediv (M.emul inc_lindex inc_lindex) inc_lindex = inc_lindex)
      >> C.success

  let () = test "smul" & fun r ->
      r
      >> (M.smul 2. lindex = db_lindex)
      >> (M.smul (-2.) lindex = M.neg db_lindex)
      >> C.success

  let () = test "transpose" & fun r ->
      r
      >> (M.transpose M.id = M.id)
      >> Cm.for_all g_m
        begin fun m r ->
          r
          >> (M.transpose (M.transpose m) = m)
          >> C.success
        end
      >> C.success

  open Cf.Order
  let () = test "trace" & fun r ->
      r
      >> (M.trace M.id = (float M.dim))
      >> (M.trace lindex = float ((M.dim * M.dim * M.dim - M.dim) / 2))
      >> C.success

  let () = test "det" & fun r ->
      r
      >> (M.det M.id = 1.0)
      >> Cm.for_all g_m
        begin fun m r ->
          let eps = 1e-6 in
          r
          >> (Float.round_zero ~eps ((M.det (M.transpose m)) -. M.det m) = 0.)
          >> C.success
        end
      >> C.success

  open Cm.Order
  let () = test "inv" & fun r ->
      let invertible m = Stdlib.(<>) (Float.round_zero ~eps (M.det m)) 0. in
      r >> (M.inv M.id = M.id)
      >> Cm.for_all ~cond:invertible g_m
        begin fun m r ->
          let id' = M.map (Float.chop ~eps) (M.mul (M.inv m) m) in
          r >> (id' = M.id)
          >> C.success
        end
      >> C.success

  (* Traversal *)

  open Cf.Order
  let () = test "map" & fun r ->
      let check r (i, j, li) = r >> (M.el i j db_lindex = float (2 * li)) in
      List.fold_left check r indices >> C.success

  let () = test "mapi" & fun r ->
      let check r (i, j, li) = r >> (M.el i j lindex = float li) in
      List.fold_left check r indices >> C.success

  open Cm.Order

  let () = test "fold" & fun r ->
      let ro = M.fold (fun acc li -> (int_of_float li) :: acc) [] lindex in
      r
      >> C.Order.(=) (List.rev ro) (List.map (fun (_, _, li) -> li) indices)
      >> C.success

  let () = test "foldi" & fun r ->
      let ro =
        M.foldi (fun acc i j li -> (i, j, int_of_float li) :: acc) [] lindex
      in
      r
      >> C.Order.(=) (List.rev ro) (indices)
      >> C.success

  let () = test "iter" & fun r ->
      let acc = ref [] in
      M.iter (fun e -> acc := int_of_float e :: !acc) lindex;
      r
      >> C.Order.(=) !acc (List.rev_map (fun (_, _, li) -> li) indices)
      >> C.success

  let () = test "iteri" & fun r ->
      let acc = ref [] in
      M.iteri (fun i j e -> acc := (i, j, int_of_float e) :: !acc) lindex;
      r
      >> C.Order.(=) (List.rev !acc) indices
      >> C.success

  (* Predicates and comparisons *)

  let () = test "for_all" & fun r ->
      r >> Cm.holds (M.for_all (fun x -> Stdlib.(=) x 0.)) M.zero
      >> C.success

  let () = test "exists" & fun r ->
      let check r (_, _, li) =
        let p m = (M.exists (fun e -> Stdlib.(=) e (float li))) m in
        r >> Cm.holds p lindex >> C.success
      in
      List.fold_left check r indices >> C.success

  open Cb.Order
  let () = test "equal_f" & fun r ->
      r
      >> C.for_all (Gen.t2 g_m g_m)
        begin fun (m, m') r ->
          r
          >> (M.equal m m = M.equal_f Stdlib.(=) m m)
          >> (M.equal m m' = M.equal_f Stdlib.(=) m m')
          >> C.success
        end
      >> C.success

  open Ci.Order
  let () = test "compare_f" & fun r ->
      r
      >> C.for_all (Gen.t2 g_m g_m)
        begin fun (m, m') r ->
          r
          >> (M.compare m m = M.compare_f Stdlib.compare m m)
          >> (M.compare m m' = M.compare_f Stdlib.compare m m')
          >> C.success
        end
      >> C.success
end

module M2_tests = struct

  module M2 = struct
    include M2
    let gen ~min ~len =
      let g _ rs =
        let r = Float.srandom rs ~min ~len in M2.v
          (r ()) (r ())
          (r ()) (r ())
      in
      g, pp
  end

  include M_tests (M2)

  open Cf.Order
  let () = test "eij" & fun r ->
      r
      >> (M2.e00 lindex = 0.)
      >> (M2.e10 lindex = 1.)
      >> (M2.e01 lindex = 2.)
      >> (M2.e11 lindex = 3.)
      >> C.success

  open Cm.Order
  let () = test "row, of_rows" & fun r ->
      r
      >> (M2.of_rows (M2.row 0 lindex) (M2.row 1 lindex) = lindex) >> C.success

  let () = test "col, of_cols" & fun r ->
      r
      >> (M2.of_cols (V2.basis 0) (V2.basis 1) = M2.id)
      >> (M2.of_cols (M2.col 0 lindex) (M2.col 1 lindex) = lindex) >> C.success

  (* 2D space transformations *)

  open V2_tests.Cv.Order
  let () = test "rot2" & fun r ->
      let m = M2.rot2 Float.pi_div_2 in
      let chop = V2.map (Float.chop ~eps) in
      r
      >> (chop (V2.ltr m V2.ox) = V2.oy)
      >> (chop (V2.ltr m V2.oy) = (V2.neg V2.ox))
      >> (chop (V2.ltr m (V2.neg V2.ox)) = (V2.neg V2.oy))
      >> (chop (V2.ltr m (V2.neg V2.oy)) = V2.ox)
      >> C.success

  let () = test "scale2" & fun r ->
      let s = V2.v 2. 3. in
      let m = M2.scale2 s in
      r >> (V2.ltr m s = V2.mul s s) >> C.success
end

module M3_tests = struct
  module M3 = struct
    include M3
    let gen ~min ~len =
      let g _ rs =
        let r = Float.srandom rs ~min ~len in M3.v
          (r ()) (r ()) (r ())
          (r ()) (r ()) (r ())
          (r ()) (r ()) (r ())
      in
      g, pp
  end

  include M_tests (M3)

  open Cf.Order
  let () = test "eij" & fun r ->
      r
      >> (M3.e00 lindex = 0.)
      >> (M3.e10 lindex = 1.)
      >> (M3.e20 lindex = 2.)
      >> (M3.e01 lindex = 3.)
      >> (M3.e11 lindex = 4.)
      >> (M3.e21 lindex = 5.)
      >> (M3.e02 lindex = 6.)
      >> (M3.e12 lindex = 7.)
      >> (M3.e22 lindex = 8.)
      >> C.success

  open Cm.Order
  let () = test "row, of_rows" & fun r ->
      r >> (M3.of_rows (M3.row 0 lindex) (M3.row 1 lindex) (M3.row 2 lindex) =
            lindex)
      >> C.success

  let () = test "col, of_cols" & fun r ->
      r
      >> (M3.of_cols (V3.basis 0) (V3.basis 1) (V3.basis 2) = M3.id)
      >> (M3.of_cols (M3.col 0 lindex) (M3.col 1 lindex) (M3.col 2 lindex) =
          lindex)
      >> C.success

  let () = test "of_m2_v2" & fun r ->
      r
      >> (M3.of_m2_v2 (M2.v 1. 2. 4. 5.) (V2.v 3. 6.) =
          M3.v 1. 2. 3. 4. 5. 6. 0. 0. 1.)
      >> C.success

  (* 2D space transformations *)

  open V2_tests.Cv.Order
  let () = test "move2" & fun r ->
      let d = (V2.v 1. 2.) in
      let m = M3.move2 d in
      r
      >> (V2.tr m d = d)
      >> (P2.tr m d = V2.smul 2. d)
      >> (P2.tr m P2.o = d)
      >> C.success

  let () = test "rot2" & fun r ->
      let chop v = V2.map (Float.chop ~eps) v in
      let m = M3.rot2 Float.pi_div_2 in
      r
      >> (chop (V2.tr m V2.ox) = V2.oy)
      >> (chop (P2.tr m V2.ox) = V2.oy)
      >> (chop (P2.tr m P2.o) = P2.o)
      >> C.success

  let () = test "scale2" & fun r ->
      let s = V2.v 2. 3. in
      let m = M3.scale2 s in
      r
      >> (V2.tr m s = V2.mul s s)
      >> (P2.tr m s = V2.mul s s)
      >> (P2.tr m P2.o = P2.o)
      >> C.success

  let () = test "rigid2" & fun r ->
      let chop v = V2.map (Float.chop ~eps) v in
      let m = (M3.rigid2 ~move:(V2.v 2. 3.) ~rot:Float.pi_div_4) in
      let m' = (M3.mul (M3.move2 (V2.v 2. 3.)) (M3.rot2 Float.pi_div_4)) in
      let ri = (M3.rigid2 ~move:(V2.v 1. 2.) ~rot:Float.pi_div_2) in
      r
      >> Cm.Order.(=) m m'
      >> (chop (V2.tr ri V2.ox) = V2.oy)
      >> (chop (P2.tr ri V2.ox) = P2.v 1. 3.)
      >> C.success

  let () = test "srigid2" & fun r ->
      let m =
        M3.srigid2 ~move:(V2.v 2. 3.) ~rot:Float.pi_div_4 ~scale:(V2.v 2. 3.)
      in
      let ri = M3.mul (M3.move2 (V2.v 2. 3.)) (M3.rot2 Float.pi_div_4) in
      let m' = M3.mul ri (M3.scale2 (V2.v 2. 3.)) in
      let cmp = M3.compare_f (Float.compare_tol ~eps) in
      r
      >> C.Order.(=) ~cmp ~pr:M3.pp m m'
      >> C.success

  (* 3D space transformations *)

  open V3_tests.Cv.Order
  let () = test "rot3_map" & fun r ->
      let m = M3.rot3_map V3.ox V3.oz in
      let chop = V3.map (Float.chop ~eps) in
      r >> (chop (V3.ltr m V3.ox) = V3.oz) >> C.success

  let () = test "rot3_axis" & fun r ->
      let m = M3.rot3_axis V3.ox Float.pi_div_2 in
      let chop = V3.map (Float.chop ~eps) in
      r >> (chop (V3.ltr m V3.oy) = V3.oz) >> C.success

  let () = test "rot3_zyx" & fun r ->
      let a = Float.pi_div_2 in
      let m = M3.rot3_zyx (V3.v a a a) in
      let chop = V3.map (Float.chop ~eps) in
      r >> (chop (V3.ltr m V3.oy) = V3.oy) >> C.success

  let () = test "scale3" & fun r ->
      let s = V3.v 2. 3. 4. in
      let m = M3.scale3 s in
      r >> (V3.ltr m s = V3.mul s s) >> C.success

end

module M4_tests = struct
  module M4 = struct
    include M4
    let gen ~min ~len =
      let g _ rs =
        let r = Float.srandom rs ~min ~len in M4.v
          (r ()) (r ()) (r ()) (r ())
          (r ()) (r ()) (r ()) (r ())
          (r ()) (r ()) (r ()) (r ())
          (r ()) (r ()) (r ()) (r ())
      in
      g, pp
  end

  include M_tests (M4)

  open Cf.Order
  let () = test "eij" & fun r ->
      r
      >> (M4.e00 lindex = 0.)
      >> (M4.e10 lindex = 1.)
      >> (M4.e20 lindex = 2.)
      >> (M4.e30 lindex = 3.)
      >> (M4.e01 lindex = 4.)
      >> (M4.e11 lindex = 5.)
      >> (M4.e21 lindex = 6.)
      >> (M4.e31 lindex = 7.)
      >> (M4.e02 lindex = 8.)
      >> (M4.e12 lindex = 9.)
      >> (M4.e22 lindex = 10.)
      >> (M4.e32 lindex = 11.)
      >> (M4.e03 lindex = 12.)
      >> (M4.e13 lindex = 13.)
      >> (M4.e23 lindex = 14.)
      >> (M4.e33 lindex = 15.)
      >> C.success

  open Cm.Order
  let () = test "row, of_rows" & fun r ->
      r
      >> (M4.of_rows (M4.row 0 lindex) (M4.row 1 lindex)
            (M4.row 2 lindex) (M4.row 3 lindex) = lindex)
      >> C.success

  let () = test "col, of_cols" & fun r ->
      r
      >> (M4.of_cols (V4.basis 0) (V4.basis 1) (V4.basis 2) (V4.basis 3)=M4.id)
      >> (M4.of_cols (M4.col 0 lindex) (M4.col 1 lindex)
            (M4.col 2 lindex) (M4.col 3 lindex) = lindex)
      >> C.success

  let () = test "of_m3_v3" & fun r ->
      r
      >> (M4.of_m3_v3 (M3.v 1. 2. 3. 5. 6. 7. 9. 10. 11.) (V3.v 4. 8. 12.) =
          M4.v 1. 2. 3. 4. 5. 6. 7. 8. 9. 10. 11. 12. 0. 0. 0. 1.)
      >> C.success

  (* 2D space transformations *)

  open V3_tests.Cv.Order
  let () = test "move2" & fun r ->
      let d = (V2.v 1. 2.) in
      let m = M4.move2 d in
      let d = (V3.of_v2 d ~z:0.) in
      r
      >> (V3.tr m d = d)
      >> (P3.tr m d = V3.smul 2. d)
      >> (P3.tr m P3.o = d)
      >> C.success

  let () = test "rot2" & fun r ->
    let chop v = V3.map (Float.chop ~eps) v in
    let m = M4.rot2 Float.pi_div_2 in
    r
    >> (chop (V3.tr m V3.ox) = V3.oy)
    >> (chop (P3.tr m V3.ox) = V3.oy)
    >> (chop (P3.tr m P3.o) = P3.o)
    >> C.success

  let () = test "scale2" & fun r ->
    let s = V2.v 2. 3. in
    let m = M4.scale2 s in
    let s = V3.of_v2 s ~z:0. in
    r
    >> (V3.tr m s = V3.mul s s)
    >> (P3.tr m s = V3.mul s s)
    >> (P3.tr m P3.o = P3.o)
    >> C.success

  let () = test "rigid2" & fun r ->
    let chop v = V3.map (Float.chop ~eps) v in
    let m = (M4.rigid2 ~move:(V2.v 2. 3.) ~rot:Float.pi_div_4) in
    let m' = (M4.mul (M4.move2 (V2.v 2. 3.)) (M4.rot2 Float.pi_div_4)) in
    let ri = (M4.rigid2 ~move:(V2.v 1. 2.) ~rot:Float.pi_div_2) in
    r
    >> Cm.Order.(=) m m'
    >> (chop (V3.tr ri V3.ox) = V3.oy)
    >> (chop (P3.tr ri V3.ox) = P3.v 1. 3. 0.)
    >> C.success

  let () = test "srigid2" & fun r ->
      let m =
        M4.srigid2 ~move:(V2.v 2. 3.) ~rot:Float.pi_div_4 ~scale:(V2.v 2. 3.)
      in
    let ri = M4.mul (M4.move2 (V2.v 2. 3.)) (M4.rot2 Float.pi_div_4) in
    let m' = M4.mul ri (M4.scale2 (V2.v 2. 3.)) in
    let cmp = M4.compare_f (Float.compare_tol ~eps) in
    r
    >> C.Order.(=) ~cmp ~pr:M4.pp m m'
    >> C.success

  (* 3D space transformations *)

  open V3_tests.Cv.Order
  let () = test "move3" & fun r ->
      let d = (V3.v 1. 2. 3.) in
      let m = M4.move3 d in
      r
      >> (V3.tr m d = d)
      >> (P3.tr m d = V3.smul 2. d)
      >> (P3.tr m P3.o = d)
      >> C.success

  let () = test "rot3_map" & fun r ->
      let m = M4.rot3_map V3.oz V3.ox in
      let chop = V3.map (Float.chop ~eps) in
      r
      >> (chop (V3.tr m V3.oz) = V3.ox)
      >> (chop (P3.tr m V3.oz) = V3.ox)
      >> (chop (P3.tr m P3.o) = P3.o)
      >> C.success

  let () = test "rot3_axis" & fun r ->
      let m = M4.rot3_axis V3.oy Float.pi_div_2 in
      let chop = V3.map (Float.chop ~eps) in
      r
      >> (chop (V3.tr m V3.oz) = V3.ox)
      >> (chop (P3.tr m V3.oz) = V3.ox)
      >> (chop (P3.tr m P3.o) = P3.o)
      >> C.success

  let () = test "rot3_zyx" & fun r ->
      let a = -. Float.pi_div_2 in
      let m = M4.rot3_zyx (V3.v a a a) in
      let chop = V3.map (Float.chop ~eps) in
      r
      >> (chop (V3.tr m V3.oy) = V3.neg V3.oy)
      >> (chop (P3.tr m V3.oy) = V3.neg V3.oy)
      >> (chop (P3.tr m P3.o) = P3.o)
      >> C.success

  let () = test "scale3" & fun r ->
      let s = V3.v 2. 3. 4. in
      let m = M4.scale3 s in
      r
      >> (V3.tr m s = V3.mul s s)
      >> (P3.tr m s = V3.mul s s)
      >> (P3.tr m P3.o = P3.o)
      >> C.success

  let () = test "rigid3, rigid3q" & fun r ->
      let v = V3.v 2. 3. 4. in
      let a = Float.pi_div_4 in
      let m = M4.rigid3 ~move:v ~rot:(V3.ox, a) in
      let mq = M4.rigid3q ~move:v ~rot:(Quat.rot3_axis V3.ox a) in
      let m' = M4.mul (M4.move3 v) (M4.rot3_axis V3.ox a) in
      let cmp = M4.compare_f (Float.compare_tol ~eps) in
      r
      >> C.Order.(=) ~cmp ~pr:M4.pp m m'
      >> C.Order.(=) ~cmp ~pr:M4.pp mq m'
      >> C.success

  let () = test "srigid3, srigid3q" & fun r ->
      let v = V3.v 2. 3. 4. in
      let a = Float.pi_div_4 in
      let m = M4.srigid3 ~move:v ~rot:(V3.ox, a) ~scale:v in
      let mq = M4.srigid3q ~move:v ~rot:(Quat.rot3_axis V3.ox a) ~scale:v in
      let ri = M4.mul (M4.move3 v) (M4.rot3_axis V3.ox a) in
      let m' = M4.mul ri (M4.scale3 v) in
      let cmp = M4.compare_f (Float.compare_tol ~eps) in
      r
      >> C.Order.(=) ~cmp ~pr:M4.pp m m'
      >> C.Order.(=) ~cmp ~pr:M4.pp mq m'
      >> C.success

  (* 4D space transformations *)

  open V4_tests.Cv.Order
  let () = test "scale4" & fun r ->
      let s = V4.v 2. 3. 4. 5. in
      let m = M4.scale4 s in
      r >> (V4.ltr m s = V4.mul s s) >> C.success

end

module Quat_tests = struct
  let test n f = Test.add_test ("Quat." ^ n) f
  let g_q  = V4_tests.g_v
  let chop3 = V3.map (Float.chop ~eps)
  let chop4 = V4.map (Float.chop ~eps)

  module Cq = V4_tests.Cv

  open Cq.Order
  let () = test "mul, conj, inv" & fun r ->           (* conj is used by inv. *)
      r
      >> Cq.for_all ~cond:(fun q ->  not (V4.equal q Quat.zero)) g_q
        begin fun q r ->
          r >> (chop4 (Quat.mul q (Quat.inv q)) = Quat.id) >> C.success
        end
      >> C.success

  let () = test "rot_map, M3.of_quat, of_m3" & fun r ->
      let m = M3.rot3_map V3.ox V3.oz in
      let q = Quat.rot3_map V3.ox V3.oz in
      let fcmp = Float.compare_tol ~eps in
      let mcmp = M3.compare_f fcmp in
      let qcmp = V4.compare_f fcmp in
      r
      >> (C.Order.(=) ~cmp:mcmp ~pr:M3.pp m (M3.of_quat q))
      >> (C.Order.(=) ~cmp:qcmp ~pr:V4.pp q (Quat.of_m3 m))
      >> (V3_tests.Cv.Order.(=) (chop3 (Quat.apply3 q V3.ox)) V3.oz)
      >> (V4_tests.Cv.Order.(=) (chop4 (Quat.apply4 q V4.ox)) V4.oz)
      >> C.success

  let () = test "rot_axis, to_rot, M3.of_quat, of_m4" & fun r ->
      let theta = Float.pi_div_2 in
      let m = M4.rot3_axis V3.ox theta in
      let q = Quat.rot3_axis V3.ox theta in
    let axis, theta' = Quat.to_rot3_axis q in
      let fcmp = Float.compare_tol ~eps in
      let mcmp = M4.compare_f fcmp in
      let vcmp = V3.compare_f fcmp in
      let qcmp = V4.compare_f fcmp in
      r
      >> (C.Order.(=) ~cmp:mcmp ~pr:M4.pp m (M4.of_quat q))
      >> (C.Order.(=) ~cmp:qcmp ~pr:V4.pp q (Quat.of_m4 m))
      >> (C.Order.(=) ~cmp:vcmp ~pr:V3.pp axis V3.ox)
      >> (C.Order.(=) ~cmp:fcmp ~pr:Format.pp_print_float theta' theta)
      >> (V3_tests.Cv.Order.(=) (chop3 (Quat.apply3 q V3.oy)) V3.oz)
      >> C.success

  let () = test "rot_zyx, to_zyx, M3.of_m3, of_m3" & fun ru ->
      let theta = Float.pi_div_2 in
      let r = V3.v (-. theta) theta theta in
      let m = M3.rot3_zyx r in
      let q = Quat.rot3_zyx r in
      let r' = Quat.to_rot3_zyx q in
      let fcmp = Float.compare_tol ~eps:1e-6 in
      let mcmp = M3.compare_f fcmp in
      let vcmp = V3.compare_f fcmp in
      let qcmp = V4.compare_f fcmp in
      ru
      >> (C.Order.(=) ~cmp:mcmp ~pr:M3.pp m (M3.of_quat q))
      >> (C.Order.(=) ~cmp:qcmp ~pr:V4.pp q (Quat.of_m3 m))
      >> (C.Order.(=) ~cmp:vcmp ~pr:V3.pp r r')
      >> (V3_tests.Cv.Order.(=) (chop3 (Quat.apply3 q V3.oy)) (V3.neg V3.oy))
      >> C.success
end

(* Size tests *)

module type Size  = sig
  include Gg.Size
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val gen : min:float -> len:float -> t Checkm.gen         (* random size. *)
end

module Size_tests (Size : Size) (V : Gg.V with type t = Size.t) = struct
  let s_prefix = "Size" ^ (string_of_int Size.dim) ^ "."
  let test n f = Test.add_test (s_prefix ^ n) f
  let g_s = Size.gen ~min:(-100.) ~len:200.
  let index = V.mapi (fun i _ -> (float i)) Size.zero

  module Cs = C.Make (Size)

  open Cs.Order
  let () = test "zero" & fun r ->
    r
    >> Cs.holds (V.for_all (fun c -> Stdlib.(=) c 0.)) Size.zero
    >> C.success

  let () = test "unit" & fun r ->
    r
    >> Cs.holds (V.for_all (fun c -> Stdlib.(=) c 1.)) Size.unit
    >> C.success
end

module Size2_tests = struct

  module Size2 = struct
    include Size2
    let compare = V2.compare
    let pp = V2.pp
    let gen = V2_tests.V2.gen
  end

  include Size_tests (Size2) (V2)

  open Cf.Order
  let () = test "w, h" & fun r ->
      r
      >> (Size2.w index = 0.)
      >> (Size2.h index = 1.)
      >> C.success

  let () = test "aspect" & fun r ->
      r
      >> (Size2.aspect (Size2.v 8. 2.) = 4.)
      >> C.success

  open Cs.Order
  let () = test "of_h, of_w" & fun r ->
      r
      >> (Size2.of_w 8. ~aspect:4. = Size2.v 8. 2.)
      >> (Size2.of_h 2. ~aspect:4. = Size2.v 8. 2.)
      >> C.success
end

module Size3_tests = struct

  module Size3 = struct
    include Size3
    let compare = V3.compare
    let pp = V3.pp
    let gen = V3_tests.V3.gen
  end

  include Size_tests (Size3) (V3)

  open Cf.Order
  let () = test "w, h, d" & fun r ->
      r
      >> (Size3.w index = 0.)
      >> (Size3.h index = 1.)
      >> (Size3.d index = 2.)
      >> C.success
end

(* Box tests. *)

module type Box  = sig
  include Gg.Box
  val gen : min:float -> len:float -> t Checkm.gen          (* random box. *)
end

module Box_tests
    (V : V)
    (P : P with type t = V.t)
    (Size : Size with type t = V.t)
    (M : M with type v = V.t)
    (Box : Box with type v = V.t and type p = P.t and type size = Size.t and
    type m = M.t)
= struct
  let p_prefix = "Box" ^ (string_of_int Box.dim) ^ "."
  let test n f = Test.add_test (p_prefix ^ n) f
  let g_box = Box.gen ~min:(-100.) ~len:200.
  let fcmp = Float.compare_tol ~eps
  let vcmp = V.compare_f fcmp
  let bcmp = Box.compare_f fcmp
  let vt = V.mapi (fun i _ -> float (i + 1)) V.zero
  let usize = V.mapi (fun i _ -> 1.) V.zero
  let one = usize
  let vth = V.(0.5 * vt)
  let vtb = Box.v P.o vt
  let bt = Box.v vt vt
  let bt_mid = Box.v_mid V.zero V.(2. * vt)
  module Cbox = C.Make (Box)
  module Cv = C.Make (V)

  open Cv.Order
  let () = test "v_mid, o, size, zero, unit" & fun r ->
      r
      >> (Box.o Box.zero = P.o)
      >> (Box.o Box.unit = P.o)
      >> (Box.o bt = vt)
      >> (Box.o bt_mid = V.neg vt)
      >> Cbox.raises_invalid_arg Box.o Box.empty
      >> (Box.size Box.zero = V.zero)
      >> (Box.size Box.unit = usize)
      >> (Box.size bt = vt)
      >> (Box.size bt_mid = V.(2. * vt))
      >> Cbox.raises_invalid_arg Box.size Box.empty
      >> C.success

  open Cv.Order
  let () = test "min, max, mid, of_pts" & fun r ->
      let max = V.add (Box.o bt) (Box.size bt) in
      let mid = V.smul 0.5 (V.add (Box.o bt) max) in
      let minbt = Box.min bt in
      let maxbt = Box.max bt in
      r
      >> (minbt = vt)
      >> (maxbt = max)
      >> C.Order.(=) ~pr:V.pp ~cmp:vcmp (Box.mid bt) mid
      >> C.Order.(=) ~pr:Box.pp ~cmp:bcmp (Box.of_pts minbt maxbt) bt
      >> C.Order.(=) ~pr:Box.pp ~cmp:bcmp (Box.of_pts maxbt minbt) bt
      >> Cbox.raises_invalid_arg Box.min Box.empty
      >> Cbox.raises_invalid_arg Box.max Box.empty
      >> Cbox.raises_invalid_arg Box.mid Box.empty
      >> C.success

  open Cbox.Order
  let () = test "add_pt" & fun r ->
      r
      >> (Box.add_pt Box.empty vt = Box.v vt Size.zero)
      >> (List.fold_left Box.add_pt Box.empty [vt; vth] = Box.v vth vth)
      >> (List.fold_left Box.add_pt Box.empty [vth; vt] = Box.v vth vth)
      >> (List.fold_left Box.add_pt Box.empty [P.o; vth; vt] = vtb)
      >> (List.fold_left Box.add_pt Box.empty [P.o; vt;  vth] = vtb)
      >> (List.fold_left Box.add_pt Box.empty [vth; P.o; vt] = vtb)
      >> (List.fold_left Box.add_pt Box.empty [vth; vt;  P.o] = vtb)
      >> (List.fold_left Box.add_pt Box.empty [vt;  P.o; vth] = vtb)
      >> (List.fold_left Box.add_pt Box.empty [vt;  vth; P.o] = vtb)
      >> C.success

  open Cf.Order
  let () = test "area" & fun r ->
      let rec fact n = if Stdlib.(<=) n 0 then 1 else n * fact (n - 1) in
      let choices n k = (fact n) / (fact k) * (fact (n - k )) in
      let usides = (choices Box.dim 2) * (1 lsl (Box.dim - 2)) in
      r >> (Box.area Box.unit = (float usides) *. 1.) >> C.success

  open Cbox.Order
  let () = test "inter, isects" & fun r ->
      let isects (b, b') = Box.isects b b' in
      let check a b c r =
      r
      >> C.Order.(=) ~pr:Box.pp ~cmp:bcmp (Box.inter a b) c
      >> C.Order.(=) ~pr:Box.pp ~cmp:bcmp (Box.inter b a) c
      >> C.holds isects (a, b)
      >> C.holds isects (b, a)
      >> C.success
      in
      let check_empty a b r =
        r
        >> (Cbox.holds Box.is_empty (Box.inter a b))
        >> (Cbox.holds Box.is_empty (Box.inter b a))
        >> C.holds (C.neg isects) (a, b)
        >> C.holds (C.neg isects) (b, a)
        >> C.success
      in
      let umid = V.smul 0.5 usize in
      let b = Box.v umid usize in
      let i = Box.v umid umid in
      r
      >> check_empty Box.unit bt
      >> check Box.unit b i
      >> check_empty Box.empty Box.empty
      >> Cbox.for_all g_box begin fun b r -> r
        >> check b b b
        >> check_empty b Box.empty
        >> C.success
      end
      >> C.success

  open Cv.Order
  let () = test "union, subset" & fun r ->
      let subset (b, b') = Box.subset b b' in
      let check a b c r =
        r
        >> C.Order.(=) ~pr:Box.pp ~cmp:bcmp (Box.union a b) c
        >> C.Order.(=) ~pr:Box.pp ~cmp:bcmp (Box.union b a) c
        >> C.holds subset (a, c)
        >> C.holds subset (b, c)
        >> C.success
      in
      let umid = V.smul 0.5 usize in
      let b = Box.v umid usize in
      let u = Box.union b bt in
      r >> check b bt u
      >> (Box.min u = umid)
      >> (Box.max u = Box.max bt)
      >> Cbox.for_all g_box begin fun b r ->
        r
        >> check b b b
        >> check b Box.empty b
        >> check Box.empty b b
        >> C.success
      end
     >> begin fun r ->
       let left = Box.v V.zero (V.smul 10. one) in
       let right = Box.v (V.smul 100. one) one in
       r
       >> C.holds (C.neg subset) (right, left)
       >> C.success
     end
     >> C.success

  open Cv.Order
  let () = test "inset, is_pt" & fun r ->
      let vmid = V.smul 0.5 usize in
      let i = Box.inset vmid Box.unit in
      let o = Box.inset (V.neg usize) Box.unit in
      let mid = Box.inset usize Box.unit in
      let mid' = Box.inset (V.smul 4.0 usize) Box.unit in
      r
      >> (Cbox.holds Box.is_pt i)
      >> (Box.min i = vmid)
      >> (Box.max i = vmid)
      >> (Box.min o = V.neg usize)
      >> (Box.max o = V.smul 2. usize)
      >> (Cbox.holds Box.is_pt mid)
      >> (Box.min mid = vmid)
      >> (Box.max mid = vmid)
      >> Cbox.holds (Box.equal mid') mid
      >> (Cbox.holds Box.is_empty (Box.inset vmid Box.empty))
      >> (Cbox.holds Box.is_empty (Box.inset (V.neg vmid) Box.empty))
      >> C.success

  open Cbox.Order
  let () = test "round" & fun r ->
      let vmid = V.smul 0.5 usize in
      let b = Box.v vmid V.zero in
      let b' = Box.v (V.neg vmid) V.zero in
      r
      >> (Box.round b = Box.unit)
      >> (Box.round b' = Box.v (V.neg usize) usize)
      >> (Box.round Box.empty = Box.empty)
      >> C.success

  let () = test "move" & fun r ->
      r >> (Box.move (V.neg vt) bt = Box.v P.o vt) >> C.success

  open Cbox.Order
  let () = test "map_f" & fun r ->
      r >> (Box.map_f (fun _ -> 0.) bt = Box.zero) >> C.success

  open Cbox.Order
  let () = test "mem" & fun r ->
      let mem (p, b) = Box.mem p b in
      let rec basis i r =
        if Stdlib.(<) i 0 then C.success r else
        let v = V.basis i in
        r
        >> (C.holds mem (v, Box.unit))
        >> (C.holds (C.neg mem) (V.neg v, Box.unit))
        >> (C.holds (C.neg mem) (V.smul 2. v, Box.unit))
        >> (C.holds (C.neg mem) (v, Box.empty))
        >> basis (i - 1)
      in
      r
      >> C.holds mem (V.zero, Box.unit)
      >> C.holds mem (usize, Box.unit)
      >> C.holds mem (V.smul 0.5 usize, Box.unit)
      >> C.holds (C.neg mem) (V.smul 2. usize, Box.unit)
      >> basis (Box.dim - 1)
      >> C.success

  open Cb.Order
  let () = test "equal_f" & fun r ->
      r
      >> C.for_all (Gen.t2 g_box g_box)
        begin fun (v, v') r ->
          r >> (Box.equal v v = Box.equal_f Stdlib.(=) v v)
          >> (Box.equal v v' = Box.equal_f Stdlib.(=) v v')
          >> C.success
        end
      >> C.success

  open Ci.Order
  let () = test "compare_f" & fun r ->
    r
    >> C.for_all (Gen.t2 g_box g_box)
      begin fun (v, v') r ->
        let e = Box.empty in
        r
        >> (Box.compare v v = Box.compare_f Stdlib.compare v v)
        >> (Box.compare v v' = Box.compare_f Stdlib.compare v v')
        >> (Box.compare v e = Box.compare_f Stdlib.compare v e)
        >> (Box.compare e v = Box.compare_f Stdlib.compare e v)
        >> C.success
      end
    >> C.success
end

module Box2_tests = struct

  module Box2 = struct
    include Box2
    let gen ~min ~len =
      let g _ rs =
        let r = Float.srandom rs ~min ~len in
        let o = P2.v (r ()) (r ()) in
        let size = Size2.v (abs_float (r ())) (abs_float (r ())) in
        Box2.v o size
      in
      g, pp
  end

  include Box_tests (V2_tests.V2) (P2_tests.P2) (Size2_tests.Size2)
      (M2_tests.M2) (Box2)

  open Cf.Order
  let () = test "ox, oy, w, h" & fun r ->
      r
      >> (Box2.ox bt = 1.) >> (Box2.oy bt = 2.)
      >> (Box2.w bt = 1.) >> (Box2.h bt = 2.)
      >> C.success

  let () = test "minx, miny, maxx, maxy, midx, midy" & fun r ->
      r
      >> (Box2.minx bt = V2.x (Box2.min bt))
      >> (Box2.miny bt = V2.y (Box2.min bt))
      >> (Box2.maxx bt = V2.x (Box2.max bt))
      >> (Box2.maxy bt = V2.y (Box2.max bt))
      >> (Box2.midx bt = V2.x (Box2.mid bt))
      >> (Box2.midy bt = V2.y (Box2.mid bt))
      >> C.success

  open Cv.Order
  let () = test "{b,m,l}{l,m,r}_pt" & fun r ->
      r
      >> (Box2.bl_pt bt = V2.v 1. 2.)
      >> (Box2.bm_pt bt = V2.v 1.5 2.)
      >> (Box2.br_pt bt = V2.v 2. 2.)
      >> (Box2.ml_pt bt = V2.v 1. 3.)
      >> (Box2.mm_pt bt = V2.v 1.5 3.)
      >> (Box2.mr_pt bt = V2.v 2. 3.)
      >> (Box2.tl_pt bt = V2.v 1. 4.)
      >> (Box2.tm_pt bt = V2.v 1.5 4.)
      >> (Box2.tr_pt bt = V2.v 2. 4.)
      >> C.success

  open Cbox.Order
  let () = test "ltr, tr" & fun r ->
      let ml = M2.rot2 Float.pi_div_2 in
      let mh = M3.rigid2 ~move:(V2.v (4.) (-1.)) ~rot:Float.pi_div_2 in
      let lb = Box2.v (P2.v  (-4.) 1.) (Size2.v 2. 1.) in
      let hb = Box2.v P2.o (Size2.v 2. 1.) in
      r
      >> C.Order.(=) ~pr:Box2.pp ~cmp:bcmp (Box2.ltr ml bt) lb
      >> C.Order.(=) ~pr:Box2.pp ~cmp:bcmp (Box2.tr mh bt) hb
      >> (Box2.ltr ml Box2.empty = Box2.empty)
      >> (Box2.tr mh Box2.empty = Box2.empty)
      >> C.success
end


module Box3_tests = struct

  module Box3 = struct
    include Box3
    let gen ~min ~len =
      let g _ rs =
        let r = Float.srandom rs ~min ~len in
        let o = P3.v (r ()) (r ()) (r ()) in
        let size =
          Size3.v (abs_float (r ())) (abs_float (r ())) (abs_float (r ()))
        in
        Box3.v o size
      in
      g, pp
  end

  include Box_tests (V3_tests.V3) (P3_tests.P3) (Size3_tests.Size3)
      (M3_tests.M3) (Box3)

  open Cf.Order
  let () = test "ox, oy, oz, w, h, d" & fun r ->
      r
      >> (Box3.ox bt = 1.) >> (Box3.oy bt = 2.) >> (Box3.oz bt = 3.)
      >> (Box3.w bt = 1.) >> (Box3.h bt = 2.) >> (Box3.d bt = 3.)
      >> C.success

  let () = test "minx, miny, minz, maxx, maxy, maxz, midx, midy, midz" & fun r->
      r
      >> (Box3.minx bt = V3.x (Box3.min bt))
      >> (Box3.miny bt = V3.y (Box3.min bt))
      >> (Box3.minz bt = V3.z (Box3.min bt))
      >> (Box3.maxx bt = V3.x (Box3.max bt))
      >> (Box3.maxy bt = V3.y (Box3.max bt))
      >> (Box3.maxz bt = V3.z (Box3.max bt))
      >> (Box3.midx bt = V3.x (Box3.mid bt))
      >> (Box3.midy bt = V3.y (Box3.mid bt))
      >> (Box3.midz bt = V3.z (Box3.mid bt))
      >> C.success


  open Cv.Order
  let () = test "{f,n}{b,t}{l,r}_pt" & fun r ->
      r
      >> (Box3.fbl_pt bt = V3.v 1. 2. 3.)
      >> (Box3.fbr_pt bt = V3.v 2. 2. 3.)
      >> (Box3.ftl_pt bt = V3.v 1. 4. 3.)
      >> (Box3.ftr_pt bt = V3.v 2. 4. 3.)
      >> (Box3.nbl_pt bt = V3.v 1. 2. 6.)
      >> (Box3.nbr_pt bt = V3.v 2. 2. 6.)
      >> (Box3.ntl_pt bt = V3.v 1. 4. 6.)
      >> (Box3.ntr_pt bt = V3.v 2. 4. 6.)
      >> C.success

  open Cf.Order
  let () = test "area, volume" & fun r ->
      r
      >> (Box3.area Box3.unit = 6.)
      >> (Box3.volume Box3.unit = 1.)
      >> C.success

  open Cbox.Order
  let () = test "ltr, tr" & fun r ->
    let theta = Float.pi_div_2 in
    let ml = M3.rot3_zyx (V3.v theta theta 0.) in
    let mh = M4.rigid3 ~move:(V3.v 0. (-1.) 0.) ~rot:(V3.oy, -. Float.pi) in
    let lb = Box3.v (P3.v  (0.) (-1.) (-1.)) (Size3.v 1. 1. 1.) in
    let hb = Box3.v (P3.v (-1.) (-1.) (-1.)) (Size3.v 1. 1. 1.) in
    r
    >> C.Order.(=) ~pr:Box3.pp ~cmp:bcmp (Box3.ltr ml Box3.unit) lb
    >> C.Order.(=) ~pr:Box3.pp ~cmp:bcmp (Box3.tr mh Box3.unit) hb
    >> (Box3.ltr ml Box3.empty = Box3.empty)
    >> (Box3.tr mh Box3.empty = Box3.empty)
    >> C.success
end

module Color_tests = struct
  let test n f = Test.add_test ("Color." ^ n) f

  module type Param = sig
    val bits : int
  end

  let c lab = V2.norm (V2.v (V3.y lab) (V3.z lab))

  let deltaE_lab lab1 lab2 =
    let cie76 = V3.norm (V3.sub lab1 lab2) in
    let dL,da,db = V3.to_tuple (V3.sub lab1 lab2) in
    let c1 = c lab1 and c2 = c lab2 in
    let dC = c1 -. c2 in
    let dH2 = da *. da +. db *. db -. dC *. dC in
    let dH2 = if dH2 < 0.0 then 0.0 else dH2 in
    let sC = 1. +. 0.045 *. c1
    and sH = 1. +. 0.015 *. c2 in
    let cie94 = sqrt (dL *. dL +. (dC *. dC /. sC) +. (dH2 /. sH)) in
    V2.v cie76 cie94

  let to_lab v = V3.of_v4 (Color.to_lab v)

  let deltaE color1 color2 =
    deltaE_lab (to_lab color1) (to_lab color2)

  let print_deltaE name fmt v =
    let dE = V2.x !v and dE94 = V2.y !v in
    Format.fprintf fmt "%s Max DeltaE = %g, Max CIE94 DeltaE = %g@\n"
      name dE dE94

  module Testable_color (P: Param) = struct
    module Cc = C.Make(struct
        type conv_to_lab = v4 -> v3
        type t = conv_to_lab * Test.run * v4
        let digits = truncate (ceil ((float P.bits) *. log(2.) /. log(10.)))
        let eps = 2. ** (float (-P.bits))
        let print_float fmt v =
          Format.fprintf fmt "%.*f" digits v
        let pp fmt (_,_,v) = V4.pp_f print_float fmt v
        let compare (conv,r,a) (_,_,b) =
          let cmp = V4.compare_f (Float.compare_tol ~eps) a b in
          if cmp = 0 then 0
          else begin
            let dE = deltaE_lab (conv a) (conv b) in
            ignore (C.log (print_deltaE "") (ref dE) r);
            cmp
          end
      end)

    let update_max maxDeltaE dE =
      let dE76,dE94 = V2.to_tuple dE in
      let max76,max94 = V2.to_tuple !maxDeltaE in
      maxDeltaE := V2.v (max dE76 max76) (max dE94 max94)

    let compare_rgb max id color1 color2 = fun r ->
      update_max max (deltaE color1 color2);
      r >> Cc.Order.(=) ~id (to_lab,r,color1) (to_lab,r,color2)

    let compare_lab max id lab1 lab2 = fun r ->
      update_max max (deltaE_lab (V3.of_v4 lab1) (V3.of_v4 lab2));
      r >> Cc.Order.(=) ~id (V3.of_v4,r,lab1) (V3.of_v4,r,lab2)
  end

  (** Lab fractional bits ~ 8 if we want 16-bit precision *)
  module Color8 = Testable_color(struct let bits = 8 end)

  (* Require 15-bit precision, so that 8-bit sRGB to 16-bit linear is accurate,
   * but allow for last bit to change.
   * The precision of ICC profiles is 16-bit too, and this inevitably leads
   * to different rounding when using built-in profiles vs when using an ICC
   * profile.
   * *)
  module Color16 = Testable_color(struct let bits = 15 end)

  (* Require 23-bit precision from roundtrips,
   * so that float32 HDR image conversions are accurate enough *)
  module Color23 = Testable_color(struct let bits = 23 end)

  type testcase = {
    srgb: Color.srgb;
    color: color;
    lab: Color.lab;
    gray: v2;
  }

  let to_testcase r' g' b' lr lg lb l a b gray =
    let alpha = 0.5 in {
      srgb = V4.v r' g' b' alpha;
      color = V4.v lr lg lb alpha;
      lab = V4.v l a b alpha;
      gray = V2.v gray alpha
    }

  let rec generate_testcases f lst =
    try
      let line = input_line f in
      let testcase =
        Scanf.sscanf line "%f,%f,%f,%f,%f,%f,%f,%f,%f,%f" to_testcase in
      generate_testcases f (testcase :: lst)
    with End_of_file -> List.rev lst

  let srgb_dEmax = ref V2.zero
  let srgb_check r t =
    r >> Color16.compare_rgb srgb_dEmax "RGB" (Color.of_srgb t.srgb) t.color

  let srgb_roundtrip color r =
    let srgb = Color.to_srgb color in
    let color' = Color.of_srgb srgb in
    r >> Color23.compare_rgb srgb_dEmax "sRGB roundtrip" color' color

  let lab_dEmax = ref V2.zero
  let lab_check r t =
    r >> Color8.compare_rgb lab_dEmax "RGB" (Color.of_lab t.lab) t.color
      >> Color8.compare_lab lab_dEmax "LAB" (Color.to_lab t.color) t.lab

  let lch_dEmax = ref V2.zero
  let lch_ab_roundtrip color r =
    let lch = Color.to_lch_ab color in
    let color' = Color.of_lch_ab lch in
    r >> Color23.compare_rgb lch_dEmax "LCh_ab roundtrip" color' color

  let lab_roundtrip color r =
    let lab = Color.to_lab color in
    let color' = Color.of_lab lab in
    r >> Color23.compare_rgb lab_dEmax "LAB roundtrip" color' color

  let luv_dEmax = ref V2.zero
  let luv_roundtrip color r =
    let luv = Color.to_luv color in
    let color' = Color.of_luv luv in
    r >> Color23.compare_rgb luv_dEmax "LUV roundtrip" color' color

  let lchuv_dEmax = ref V2.zero
  let lchuv_roundtrip color r =
    let lchuv = Color.to_lch_uv color in
    let color' = Color.of_lch_uv lchuv in
    r >> Color23.compare_rgb lchuv_dEmax "LCh_uv roundtrip" color' color

  let run_checks testcases f r = List.fold_left f r testcases

  let color_gen = V4_tests.V4.gen ~min:0. ~len:1.

  let () =
    let f = open_in "test/rgbtest.csv" in
    ignore (input_line f);(* header *)
    let testcases = generate_testcases f [] in
    begin test "of_srgba, to_srgba (testcases)" & fun r ->
        srgb_dEmax := V2.zero;
        r
        >> run_checks testcases srgb_check
        >> C.log (print_deltaE "sRGB testcases") srgb_dEmax
        >> C.success
    end;
    begin test "of_srgba, to_srgba (roundtrip)" & fun r ->
        srgb_dEmax := V2.zero;
        C.for_all color_gen srgb_roundtrip r >>
        C.log (print_deltaE "sRGB <-> RGB") srgb_dEmax >>
        C.success
    end;
    begin test "of_lab, to_lab (testcases)" & fun r ->
        lab_dEmax := V2.zero;
        r
        >> run_checks testcases lab_check
        >> C.log (print_deltaE "LAB testcases") lab_dEmax
        >> C.success
    end;
    begin test "of_lab, to_lab (roundtrip)" & fun r ->
        lab_dEmax := V2.zero;
        C.for_all color_gen lab_roundtrip r >>
        C.log (print_deltaE "LAB <-> RGB") lab_dEmax >>
        C.success
    end;
    begin test "of_lch, to_lch (roundtrip)" & fun r ->
        lch_dEmax := V2.zero;
        r >> C.for_all color_gen lch_ab_roundtrip
        >> C.log (print_deltaE "LCH <-> RGB") lch_dEmax
        >> C.success
    end;
    begin test "of_luv, to_luv (roundtrip)" & fun r ->
        luv_dEmax := V2.zero;
        C.for_all color_gen luv_roundtrip r >>
        C.log (print_deltaE "LUV <-> RGB") luv_dEmax >>
        C.success
    end;
    begin test "of_luv (lch), to_luv (lch) (roundtrip)" & fun r ->
        lchuv_dEmax := V2.zero;
        C.for_all color_gen lchuv_roundtrip r >>
        C.log (print_deltaE "LCH_uv <-> RGB") lchuv_dEmax >>
        C.success
    end;
    close_in f
end

(* main *)

let main () =
  let usage = Printf.sprintf
      "Usage: %s <options>\n\
       Gg's test suite. Without any options runs all tests.\nOptions:"
      (Filename.basename Sys.executable_name)
  in
  let run_conf = ref (Test.run_conf) in
  let logger = ref (Test.term_log) in
  let options = (Test.log_args logger) @ (Test.run_conf_args run_conf) in
  Arg.parse options (fun _ -> ()) usage;
  match Test.run (!logger Format.std_formatter) !run_conf !Test.list with
  | `Ok -> exit 0
  | `Fail -> exit 1

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
