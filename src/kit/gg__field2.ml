(*---------------------------------------------------------------------------
   Copyright (c) 2023 The gg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg

let invalid_argf fmt = Printf.ksprintf invalid_arg fmt

type 'a t = { dom : Box2.t; iw : int; ih : int; values : 'a array; }
let make ~dom ~iw ~ih values =
  if iw * ih = Array.length values then { dom; iw; ih; values } else
  invalid_argf "grid size mimatch %d × %d <> %d" iw ih (Array.length values)

let _of_fun ~dom f ~iw ~ih =
  let w = Box2.w dom and h = Box2.h dom in
  let ox = Box2.minx dom and oy = Box2.maxy dom in
  let xmax = float (iw - 1) and ymax = float (ih - 1) in
  let init k =
    let nx = (float (k mod iw)) /. xmax in
    let ny = (float (k / iw)) /. ymax in
    let x = ox +. w *. nx in
    let y = oy -. h *. ny in
    f (P2.v x y)
  in
  { iw; ih; dom; values = Array.init (iw * ih) init }

let of_fun ~dom f ~dpu =
  let isize = V2.(dpu * Box2.size dom) in
  let iw = Float.to_int @@ Float.ceil (Size2.w isize) in
  let ih = Float.to_int @@ Float.ceil (Size2.h isize) in
  _of_fun ~dom f ~iw ~ih

let of_fun' ?iw ?ih ~dom f =
  let ih_of_iw iw ~aspect = Float.to_int (Float.ceil (float iw) /. aspect) in
  let iw_of_ih ih ~aspect = Float.to_int (Float.ceil (float ih) *. aspect) in
  let iw, ih = match iw, ih with
  | Some iw, Some ih -> iw, ih
  | Some iw, None -> iw, ih_of_iw iw ~aspect:(Box2.aspect dom)
  | None, Some ih -> iw_of_ih ih ~aspect:(Box2.aspect dom), ih
  | None, None ->
      let aspect = Box2.aspect dom and s = 10_000 in
      if aspect < 1. then iw_of_ih s ~aspect, s else s, ih_of_iw s ~aspect
  in
  _of_fun ~dom f ~iw ~ih

let values field = field.values
let iw field = field.iw
let ih field = field.ih
let isize field = Size2.v (float field.iw) (float field.ih)
let[@inline] valuei field ~xi ~yi = field.values.(field.iw * yi + xi)
let dom field = field.dom
let dpu field =
  let hdpu = (float (iw field)) /. Box2.w field.dom in
  let vdpu = (float (ih field)) /. Box2.h field.dom in
  Size2.v hdpu vdpu

let map f field = { field with values = Array.map f field.values }
let mapi f field =
  let fk field k v =
    let xi = k mod field.iw in
    let yi = k / field.iw in
    f ~xi ~yi v
  in
  { field with values = Array.mapi (fk field) field.values }

let mapd f field =
  let xmax = float (field.iw - 1) and ymax = float (field.ih - 1) in
  let dox = Box2.minx field.dom and doy = Box2.maxy field.dom in
  let dw = Box2.w field.dom and dh = Box2.h field.dom in
  let fk field k v =
    let nx = (float (k mod field.iw)) /. xmax in
    let ny = (float (k / field.iw)) /. ymax in
    let x = dox +. dw *. nx in
    let y = doy -. dh *. ny in
    f (P2.v x y) v
  in
  { field with values = Array.mapi (fk field) field.values }

let map_values f field =
  let values = f field.values in
  let vlen = Array.length values and flen = Array.length field.values in
  if vlen = flen then { field with values} else
  invalid_argf "result array length mimatch %d <> %d" vlen flen

(* Isolines *)

let iso_pos ~smooth ~w ~iso z0 z1 =
  (* finds the iso position in the [0;w] interval. *)
  if not smooth then w *. 0.5 else
  let dz = z1 -. z0 in
  if not (Float.is_finite dz) then w *. 0.5 else w *. ((iso -. z0) /. dz)

type contour =
  { mutable first : P2.t list; (* head is first point of the polyline *)
    mutable last : P2.t list; (* head is last point of the polyline. *)
    mutable first_slot : int; (* slot where first lies (or -1) *)
    mutable last_slot : int; (* slot where last lies (or -1) *) }

let no_slot = -1
let no_contour =
  { first = []; last = []; first_slot = no_slot; last_slot = no_slot }

let new_contour first last ~first_slot ~last_slot =
  { first = [first]; last = [last]; first_slot; last_slot }

let close_contour c = List.rev_append c.last c.first

let same_contour c0 c1 = c0 == c1
let add_first c p ~slot = c.first <- p :: c.first; c.first_slot <- slot
let add_last c p ~slot = c.last <- p :: c.last; c.last_slot <- slot
let append slots c ~before =
  before.first <- List.rev_append c.last before.first;
  before.first <- List.rev_append (List.rev c.first) before.first;
  before.first_slot <- c.first_slot;
  slots.(c.first_slot) <- before

(* For enumerating the cases we build the case number by starting
   from top left (z3, msb) to bottom left (z0, lsb)

     (xi,yi) z3 o---o z2
                |   |
             z0 o---o z1 (xi+1,yi+1)

   We orient generated segments so that enclosed area >= iso is
   oriented ccw. The (xi,yi) coordinates are from the perspective of
   the array of samples not the field domain, top left z3 is (xi,yi).

   For the cases see https://en.wikipedia.org/wiki/Marching_squares\
                     #/media/File:Marching_squares_algorithm.svg

   - At the boundaries we assume everything neg_infinity, i.e.
     below the iso level. This makes the isolines closed.
   - Since we go from left to right and top to bottom this means that
     when we hit a left point we already saw it as a right point. And
     when we hit a top point we already saw it as a bottom point.
   - For stitching contours we keep a [slots] buffer that remembers the
     open contours from the previous line that we could stitch to
     on top of our cell. We also remember in [contour_prev] a possible
     contour we could stitch from the previous cell on the left.
   - Contours are deques so that we can add more at the end or at the
     beginning. We also remember in contours in which slots its current
     endpoints are (if any) as the slot may need updating when we append
     contours. *)

let isoline ?(smooth = true) f iso =
  let vs = f.values and ih = f.ih and iw = f.iw and acc = ref [] in
  let slots = Array.make (iw + 1) no_contour in
  let contour_prev = ref no_contour in
  let xmin = Box2.minx f.dom and ymax = Box2.maxy f.dom in
  let dx = Box2.w f.dom /. (float iw -. 1.) in
  let dy = Box2.h f.dom /. (float ih -. 1.) in
  for yi = -1 to ih - 1 do
    let y = ymax -. float yi *. dy and yn = yi + 1 in
    let ri = yi * iw and rn = yn * iw in
    contour_prev := no_contour;
    for xi = -1 to iw - 1 do
      let x = xmin +. float xi *. dx and xn = xi + 1 in
      let z3 = if yi < 0 || xi < 0 then neg_infinity else vs.(ri + xi) in
      let z2 = if yi < 0 || xn = iw then neg_infinity else vs.(ri + xn) in
      let z1 = if yn = ih || xn = iw then neg_infinity else vs.(rn + xn) in
      let z0 = if yn = ih || xi < 0 then neg_infinity else vs.(rn + xi) in
      let case =
        ((if z3 >= iso then 1 else 0) lsl 3) lor
        ((if z2 >= iso then 1 else 0) lsl 2) lor
        ((if z1 >= iso then 1 else 0) lsl 1) lor
        ((if z0 >= iso then 1 else 0))
      in
      let above = xi + 1 (* We have one slot for -1 *) in
      (* N.B. the [r] and [b] values are always the same and copy-pasted. *)
      match case with
      | 0 -> ()
      | 1 ->
          let b = P2.v (x +. iso_pos ~smooth ~w:dx ~iso z0 z1) (y -. dy) in
          let c = !contour_prev in
          add_first ~slot:above c b;
          contour_prev := no_contour; slots.(above) <- c;
      | 2 ->
          let r = P2.v (x +. dx) (y -. iso_pos ~smooth ~w:dy ~iso z2 z1) in
          let b = P2.v (x +. iso_pos ~smooth ~w:dx ~iso z0 z1) (y -. dy) in
          let c = new_contour r b ~first_slot:no_slot ~last_slot:above in
          contour_prev := c; slots.(above) <- c;
      | 3 ->
          let r = P2.v (x +. dx) (y -. iso_pos ~smooth ~w:dy ~iso z2 z1) in
          add_first !contour_prev r ~slot:no_slot;
      | 4 ->
          let r = P2.v (x +. dx) (y -. iso_pos ~smooth ~w:dy ~iso z2 z1) in
          let c = slots.(above) in
          add_last c r ~slot:no_slot;
          contour_prev := c; slots.(above) <- no_contour;
      | 5 ->
          (if same_contour !contour_prev slots.(above)
           then (acc := (close_contour !contour_prev) :: !acc)
           else (append slots slots.(above) ~before:!contour_prev));
          let r = P2.v (x +. dx) (y -. iso_pos ~smooth ~w:dy ~iso z2 z1) in
          let b = P2.v (x +. iso_pos ~w:dx ~smooth ~iso z0 z1) (y -. dy) in
          let c = new_contour b r ~first_slot:above ~last_slot:no_slot in
          contour_prev := c; slots.(above) <- c;
      | 6 ->
          let b = P2.v (x +. iso_pos ~smooth ~w:dx ~iso z0 z1) (y -. dy) in
          add_last slots.(above) b ~slot:above
      | 7 ->
          (if same_contour !contour_prev slots.(above)
           then (acc := (close_contour !contour_prev) :: !acc)
           else (append slots slots.(above) ~before:!contour_prev));
          contour_prev := no_contour; slots.(above) <- no_contour;
      | 8 ->
          (if same_contour !contour_prev slots.(above)
           then (acc := (close_contour !contour_prev) :: !acc)
             else (append slots !contour_prev ~before:slots.(above)));
          contour_prev := no_contour; slots.(above) <- no_contour;
      | 9 ->
          let b = P2.v (x +. iso_pos ~smooth ~w:dx ~iso z0 z1) (y -. dy) in
            add_first slots.(above) b ~slot:above;
      | 10 ->
          let r = P2.v (x +. dx) (y -. iso_pos ~smooth ~w:dy ~iso z2 z1) in
          let b = P2.v (x +. iso_pos ~smooth ~w:dx ~iso z0 z1) (y -. dy) in
          let c = !contour_prev in
          add_last c b ~slot:above;
          add_first slots.(above) r ~slot:no_slot;
          contour_prev := slots.(above); slots.(above) <- c
      | 11 ->
          let r = P2.v (x +. dx) (y -. iso_pos ~smooth ~w:dy ~iso z2 z1) in
          add_first slots.(above) r ~slot:no_slot;
          contour_prev := slots.(above); slots.(above) <- no_contour;
      | 12 ->
          let r = P2.v (x +. dx) (y -. iso_pos ~smooth ~w:dy ~iso z2 z1) in
          add_last !contour_prev r ~slot:no_slot;
      | 13 ->
          let r = P2.v (x +. dx) (y -. iso_pos ~smooth ~w:dy ~iso z2 z1) in
          let b = P2.v (x +. iso_pos ~smooth ~w:dx ~iso z0 z1) (y -. dy) in
          let c = new_contour b r ~first_slot:above ~last_slot:no_slot in
          contour_prev := c; slots.(above) <- c;
      | 14 ->
          let b = P2.v (x +. iso_pos ~smooth ~w:dx ~iso z0 z1) (y -. dy) in
          let c = !contour_prev in
          add_last c b ~slot:above;
          contour_prev := no_contour; slots.(above) <- c;
      | 15 -> ()
      | _ -> assert false
    done;
  done;
  (* The rev dance is still needed for js_of_ocaml *)
  Gg__pgon2.of_rings (List.rev_map Gg__ring2.of_pts !acc)

type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let to_rgba ?(srgb = true) field ~color ~iw ~ih (img : bigbytes) =
  let open Bigarray in
  let vs = field.values and fw = field.iw and fh = field.ih in
  if fw = iw && fh = ih then begin
    (* No scaling, can shave a few ms *)
    for i = 0 to Array.length vs - 1 do
      let color = color vs.(i) in
      let c = if srgb then Color.to_srgb color else color in
      let p = i * 4 in
      Array1.set img (p  ) (truncate (255. *. (Color.r c) +. 0.5));
      Array1.set img (p+1) (truncate (255. *. (Color.g c) +. 0.5));
      Array1.set img (p+2) (truncate (255. *. (Color.b c) +. 0.5));
      Array1.set img (p+3) (truncate (255. *. (Color.a c) +. 0.5));
    done
  end else
  let xr = float (fw - 1) /. float (iw - 1) in
  let yr = float (fh - 1) /. float (ih - 1) in
  for yi = 0 to ih - 1 do
    let row = 4 * yi * iw in
    let fy = Float.to_int (Float.round (float yi) *. yr) in
    for xi = 0 to iw - 1 do
      let fx = Float.to_int (Float.round (float xi) *. xr) in
      let v = vs.(fy * fw + fx) in
      let c = color v in
      let c = if srgb then Color.to_srgb c else c in
      let p = row + 4 * xi in
      Array1.set img (p  ) (truncate (255. *. (Color.r c) +. 0.5));
      Array1.set img (p+1) (truncate (255. *. (Color.g c) +. 0.5));
      Array1.set img (p+2) (truncate (255. *. (Color.b c) +. 0.5));
      Array1.set img (p+3) (truncate (255. *. (Color.a c) +. 0.5));
    done
  done
