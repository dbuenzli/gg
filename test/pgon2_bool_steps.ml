(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit
open Vg
open Brr_canvas
open Brr

(* The tested polygons *)

let test = List.hd Pgon2_test_cases.list
let step_op = Pgon2.Inter

(* This stuff should be added to a lib *)

let qual ~a =
  (* http://colorbrewer2.org/#type=qualitative&scheme=Accent&n=7 *)
  [| Color.v_srgbi ~a 127 201 127;
     Color.v_srgbi ~a 190 174 212;
     Color.v_srgbi ~a 253 192 134;
     Color.v_srgbi ~a 255 255 153;
     Color.v_srgbi ~a 56 108 176;
     Color.v_srgbi ~a 240 2 127;
     Color.v_srgbi ~a 191 91 23;
     (* Additional used for meta, pts *)
     Color.v_srgbi ~a 228 26 28; |]

let qual = qual ~a:0.5

let green = qual.(0)
let purple = qual.(1)
let yellowish = qual.(6) (* segment above *)
let pink = qual.(5) (* segment below *)


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

let h_stack ~gutter imgs = (* Start at P2.o and bottom-aligned on ox *)
  let rec loop gutter maxh pos acc = function
  | [] -> Box2.v P2.o (Size2.v (P2.x pos) maxh), acc
  | (b, i) :: is ->
      let tr = V2.(pos - Box2.o b) in
      let acc = I.blend (I.move tr i) acc in
      let dx = Box2.w b +. if is = [] then 0. else gutter in
      let pos = V2.v (P2.x pos +. dx) (P2.y pos) in
      let maxh = Float.max maxh (Box2.h b) in
      loop gutter maxh pos acc is
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

let cut_pgon ?(o = I.const Color.black) ?(area = `Aeo) ~w:width p i =
  let outline = `O { P.o with width } in
  let p = pgon_path p in
  I.cut ~area p i |> I.blend (I.cut ~area:outline p o)

let cut_box ~w:width b color =
  let outline = `O { P.o with width } in
  I.cut ~area:outline (P.rect b P.empty) (I.const color)

let cut_seg ~w:width p0 p1 color =
  let area = `O { P.o with width } in
  I.cut ~area (P.empty |> P.sub p0 |> P.line p1) (I.const color)

(* Algorithm images *)

let cut_sweep_line ~w:width ~x ~box color =
  let dashes = Some (0., [width; width]) in
  let area = `O { P.o with width; dashes } in
  let l0 = V2.v x (Box2.miny box) and l1 = V2.v x (Box2.maxy box) in
  I.cut ~area (P.empty |> P.sub l0 |> P.line l1) (I.const color)

let cut_sweep_result ~w evs =
  let add acc (e : Pgon2.Event.t) =
    if not e.is_left then acc else
    let color (e : Pgon2.Event.t) = if e.in_result then green else qual.(2) in
    let p0 = e.pt and p1 = e.other.pt in
    acc
    |> I.blend (cut_seg ~w p0 p1 Color.black)
    |> I.blend (cut_pt ~w:(5. *. w) p0 (color e))
    |> I.blend (cut_pt ~w:(5. *. w) p1 (color e.other))
  in
  List.fold_left add I.void evs

let cut_step_event ~w ~box (ev : Pgon2.Event.t) =
  let p0 = ev.pt and p1 = ev.other.pt in
  let sweep_line = cut_sweep_line ~w:(w *. 0.5) ~x:(P2.x p0) ~box Color.black in
  let seg_w = if ev.in_result || ev.other.in_result then 8. *. w else w in
  let linec = if ev.polygon = Pgon2.Subject then green else qual.(1) in
  sweep_line
  |> I.blend (cut_seg ~w:seg_w p0 p1 linec)
  |> I.blend (cut_pt ~w:(8. *. w) p0 linec)
  |> I.blend (cut_pt ~w:(8. *. w) p1 linec)

let cut_step ~w ~box below ev above ss =
  let cut_neighbor ~is_above = function
  | None -> I.void
  | Some (ev : Pgon2.Event.t) ->
      let c = if is_above then yellowish else pink in
      cut_seg ~w:(4. *. w) ev.pt ev.other.pt c
  in
  let status_others =
    let ev = Some ev and eq = Option.equal ( == ) in
    let add acc (e : Pgon2.Event.t) =
      let se = Some e in
      if eq se below || eq se ev || eq se above then acc else
      I.blend (cut_seg ~w:(4. *. w) e.pt e.other.pt (Color.gray ~a:0.2 0.)) acc
    in
    List.fold_left add I.void ss
  in
  status_others
  |> I.blend (cut_step_event ~w ~box ev)
  |> I.blend (cut_neighbor ~is_above:false below)
  |> I.blend (cut_neighbor ~is_above:true above)

let dump_contour ppf c =
  let pp_v2 ppf pt = Format.fprintf ppf "%.17f, %.17f" (V2.x pt) (V2.y pt) in
  let pts = List.rev (Pgon2.Contour.fold_pts List.cons c []) in
  let pp_sep ppf () = Format.fprintf ppf ";@," in
  Format.fprintf ppf "@[<v1>[%a]@]" (Format.pp_print_list ~pp_sep pp_v2) pts

let dump_pgon ppf p =
  let pp_sep ppf () = Format.fprintf ppf ";@," in
  let cs = List.rev (Pgon2.fold_contours List.cons p []) in
  Format.fprintf ppf "@[<v1>[%a]@]"
    (Format.pp_print_list ~pp_sep dump_contour) cs

let dump_result r =
  Format.(printf "@[<v>Results:@, @[<v>%a@]@]@."
            (pp_print_list Pgon2.Event.pp_dump) r)

let retract r =
  let v = match r with Ok (v, _) -> v | Error ((v, _), _) -> v in
  let cpts c = Pgon2.Contour.fold_pts (fun _ acc -> acc + 1) c 0 in
  let cs = Pgon2.fold_contours (fun c acc -> cpts c :: acc) v [] in
  Format.printf "@[<v>Result contours counts: @[<h>[%a]@]@,Result:@,%a@]@."
    Format.(pp_print_list ~pp_sep:pp_print_space pp_print_int) cs dump_pgon v;
  v

let img p0 p1 step_op =
  let box, gutter =
    let box = Box2.union (Pgon2.box p0) (Pgon2.box p1) in
    let margin = V2.(0.1 * Box2.size box) in
    Box2.outset margin box, Size2.h margin
  in
  let w = 0.0025 *. Float.min (Box2.w box) (Box2.h box) in
  let h = h_stack ~gutter and v l = v_stack ~gutter (List.rev l) in
  let res = Pgon2.Sweep.debug_result step_op p0 p1 in
  dump_result res;
  let res = box, cut_sweep_result ~w res in
  let res' =
    box, cut_sweep_result ~w
      (Pgon2.Sweep.debug_result ~filtered:false step_op p0 p1)
  in
  let op =
    let cut_pgon p = cut_pgon ~w p (I.const (Color.gray 0.9 ~a:0.75)) in
    match step_op with
    | Pgon2.Union -> box, cut_pgon (retract (Pgon2.union p0 p1))
    | Pgon2.Inter -> box, cut_pgon (retract (Pgon2.inter p0 p1))
    | Pgon2.Diff -> box, cut_pgon (retract (Pgon2.diff p0 p1))
    | Pgon2.Xor -> box, cut_pgon (retract (Pgon2.xor p0 p1))
  in
  (* ^ are constant images, let's not recompute them every step *)
  fun step ->
    let step = match step with
    | None -> I.void | Some (b, ev, a, ss) -> cut_step ~w ~box b ev a ss
    in
    let cut_arg ~w p col =
      let o = I.const (Color.with_a col 1.0) and a = I.const col in
      cut_pgon ~w ~o p a
    in
    let p0 = cut_arg ~w p0 green in
    let p1 = cut_arg ~w p1 purple in
    let step = box, p0 |> I.blend p1 |> I.blend step in
    v [ h [step; res];
        h [op; res']]

let stepper op p0 p1 =
  let step = ref (Pgon2.Sweep.debug_stepper op p0 p1) in
  fun () -> match !step () with
  | None -> step := Pgon2.Sweep.debug_stepper op p0 p1; None
  | Some _ as v -> v

let render r (view, img) =
  let aspect = Size2.aspect (Box2.size view) in
  let size = Size2.of_w 200. (* mm *) ~aspect in
  ignore (Vgr.render r (`Image (size, view, img)))

let main () =
  let body = (Document.body G.document) in
  let c = Canvas.create [] in
  let () = El.append_children body [Canvas.to_el c] in
  let r =
    let t = Vgr_htmlc.target ~resize:true (Obj.magic c) in
    Vg.Vgr.create t `Other
  in
  let _, p0, p1, _, _ = test in
  let img = img p0 p1 step_op in
  let step = stepper step_op p0 p1 in
  let render_step () = render r (img (step ())) in
  let () =
    let on_click _ = render_step () in
    let on_keydown v = render_step () in
    Ev.listen Ev.click on_click (El.as_target body);
    Ev.listen Ev.keydown on_keydown (El.as_target body);
  in
  render r (img None);
  ()

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
