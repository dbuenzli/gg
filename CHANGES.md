
- Add `P2.orient`.
- Add `P2.seg_inter`.
- Add `Float.seg_inter`.
- Add `Box{1,2,3}.outset`.

v1.0.0 2022-02-15 La Forclaz (VS)
--------------------------------

- Require OCaml 4.08.

- Handle `Pervasives`'s deprecation (and thus provide OCaml
  5.00 support).

- Drop dependency on `bigarray`'s ocamlfind package (and thus
  provide OCaml 5.00 support).
  
- Change the semantics of `Box{1,2,3}.inset`. Rather than return the
  empty box when the size in a dimension `i` become negative, clamp it
  to `0` and use the `i`th coordinate of the mid point of the box for
  the `i`th coordinate of the resulting box's origin. This means that
  insetting boxes with large values eventually degenerates to the mid
  point of a box instead of the empty box. This avoids losing a box's
  location when one grows and shrinks them arbitrarily, e.g. in
  reaction to user input. Thanks to Michel Schinz for suggesting this
  better semantics.

- Change `Gg.Float.pp` hexadecimal notation renderer to use the
  built-in `"%h"` string introduced in OCaml 4.03.0. Nans, zeros and
  infinities will render differently. Use the deprecated
  `Gg.Float.pp_legacy` if you need to recover the old hex rendering.

- The `Gg.Float` module now includes `Stdlib.Float` (#19). Some values
  initially implemented in `Gg.Float` now use `Stdlib.Float`'s
  definition or are deprecated in favour of corresponding
  functionality named differently. Implementations may differ but this
  shouldn't matter most of the time except for the first three items
  in this list:

  * **WARNING** `Gg.Float.equal` is deleted in favour of `Stdlib.Float.equal`
    The implemention differs, it moves from `x = y`
    to `compare x y = 0` which differs on nan values. 
    `Stdlib.Float.equal` treats them as equal `Gg.Float.equal` does not.
  * **WARNING** `Gg.Float.round` is deleted and becomes `Stdlib.Float.round`.
    The implementation and behaviour on negative numbers differs. 
    `Gg.Float.round` always rounded towards positive infinity on ties (`-2.` 
    on `-2.5`). `Stdlib.Float.round`s away from zero on ties (`-3.` on `-2.5`).
  * **WARNING** `Gg.Float.round_to_int` is affected by the new `round`
    implementation (see previous point).
  * `Gg.Float.compare` is deleted and becomes `Stdlib.Float.compare`
    (same implementation).
  * `Gg.Float.pi` is deleted and becomes `Stdlib.Float.pi`, the bit pattern
    of the value is unchanged.
  * `Gg.Float.is_inf` is implemented by `Stdlib.Float.is_infinite` 
    and deprecated in favour of it (different implementation). 
  * `Gg.Float.is_int` is implemented by `Stdlib.Float.is_integer`
    and deprecated in favour of it (different implementation).
  * `Gg.Float.is_nan` is deleted and becomes `Stdlib.Float.is_nan` 
    (same implementation)
  * `Gg.Float.fmax` is implemented by `Stdlib.Float.max_num` and
    deprecated in favour of it. The result of `Gg.Float.fmax (-0.)
    (+0.)` is changed, it returns `+0.` instead of `-0.`.
  * `Gg.Float.fmin` is implemented by `Stdlib.Float.min_num` and
    deprecated in favour of it. The result of `Gg.Float.fmin (+0.)
    (-0.)` is changed, it returns `-0.` instead of `+0.`.
  * `Gg.Float.sign_bit` is deleted and becomes `Stdlib.Float.sign_bit`
    (different implementation).
  * `Gg.Float.succ` is deleted and becomes `Stdlib.Float.succ` 
    (different implementation). 
  * `Gg.Float.pred` is deleted and becomes `Stdlib.Float.pred` 
    (different implementation). 
  * `Gg.Float.nan` is renamed to `Gg.Float.nan_with_payload` 
    to leave room for `Stdlib.Float.nan`'s constant.
    

v0.9.3 2018-10-23 Zagreb
------------------------

- Add `Color.to_srgbi` (inverse of `Color.v_srgbi`). Thanks to
  Christophe Troestler for the patch.
- Add missing constraints on `Float.{is_nan,equal,compare}`. Polymorphic
  equality was being used. Thanks to Christophe Troestler for the report
  (#17).
- Fix bug in `Gg.M3.rot2 ?pt:(Some _)` (#18).

v0.9.2 2017-01-24 La Forclaz (VS)
---------------------------------

- Add `Box{1,2,3}.add_pt`. Thanks to Christophe Troestler for the suggestion.
- `V{2,3,4}.norm` avoid {under,over}flows. Thanks to Christophe Troestler for
  the report and guidance.
- Fix `Size.of_w`. Thanks to @rand00 for the report and the fix.
- Safe-string support.
- Build depend on topkg.
- Relicense from BSD3 to ISC.

v0.9.1 2015-08-14 Cambridge (UK)
--------------------------------

- Fix `Box1.pp` and add to toplevel support.
- Fix broken `Box{1,2,3}.subset` functions. Thanks to Armaël Guéneau
  for the report.
- Change toplevel support scheme, `#require "gg"` no longer automatically
  opens `Gg` and installs printers. You now have to `#require "gg.top"` for
  this to happen.


v0.9.0 2014-08-23 Cambridge (UK)
--------------------------------

- Fix toplevel printer installation.
- Use package builder topkg for distribution.
- Add `Gg.Ba` (experimental). Convenience module for linear 1D bigarrays.
  The library now depends on Bigarrays.
- Many changes and fixes to the experimental `Gg.Raster` module.
- Removed `to_string` functions, they were not thread safe and we
  now have `Format.asprintf` which can be used with the pretty printers.
- Add an optional argument to `M3.rot2` to specify a center for the rotation.
- Optimize `M{2,3,4}.mul` on `M{2,3,4}.id` arguments.
- Add `M4.{move2,rot2,scale2,rigid2}`
- Rename `M4.{ortho,persp}`, replace `~bottom` by `~bot`.
- Add `Size1` module for sizes in 1D space.
- Add `Box1` module for 1D axis-aligned boxes (closed intervals).
- Add `Size2.{aspect,of_h,of_w}` functions.
- Add `Box2.{bm_pt,ml_pt,mm_pt,mr_pt,tm_pt}` for accessing middle points on
  the sides.
- Rename `Box2.{bottom,top}_{left,right}` to `Box2.{b,t}{l,r}_pt`.
- Add `Box3.{fbl,fbr,ftl,ftr,nbl,nbr,ntl,ntr}` corner accessors.
- Fix a bug in `Box3.inset`, new size was incorrectly computed.

The following functions were renamed so that each module uses the same
name for the same transform. Previously we had e.g. `M3.scale` and
`M4.scale3` (3D) and `M4.scale` (4D) which is confusing and
inconvenient when one wants to switch from one matrix to the other.

- Rename `M2.{rot,scale}` to `M2.{rot2,scale2}`.
- Rename `M3.{move,rot,rigid,srigid}` to `M3.{move2,rot2,rigid2,srigid2}`.
- Rename `M3.{rot_{map,axis,zyx},scale}` to `M3.{rot3_{map,axis,zyx},scale3}`.
- Rename `M4.{move,rot_{map,axis,zyx},scale,rigid,rigidq,srigid,srigidq}` to
  `M3.{move3,rot3_{map,axis,zyx},scale,rigid3,rigid3q,srigid3,srigid3q}`
- Rename `M4.scale` to `M4.scale4`
- Rename `M4.{rot_{map,axis,zyx},to_rot_{axis,zyx}` to
  `M4.{rot3_{map,axis,zyx},to_rot3_{axis,zyx}`.

v0.8.0 2013-09-24 Lausanne
--------------------------

First release.
Part of the work was sponsored by Citrix Systems R&D and OCaml Labs.
