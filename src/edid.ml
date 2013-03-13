let read_u16_le str pos =
  let lo = Char.code str.[pos]
  and hi = Char.code str.[pos+1] in
  (hi lsr 8) lor lo

let edid_header = "\x00\xFF\xFF\xFF\xFF\xFF\xFF\x00"

let parse_manufacturer edid =
  let b1 = Char.code edid.[8] and b2 = Char.code edid.[9] in
  let c1 = b1 lsr 2
  and c2 = ((b1 land 0x3) lsl 3) lor (b2 lsr 5)
  and c3 = b2 land 0x1f in
  let s = String.create 3 in
  s.[0] <- Char.chr (c1 + 0x40);
  s.[1] <- Char.chr (c2 + 0x40);
  s.[2] <- Char.chr (c3 + 0x40);
  s

let parse_gamma edid =
  let g = Char.code edid.[0x17] in
  if g = 0xff then None
  else Some ((float (g + 100)) /. 100.)

type primaries = {
    xr: float; yr: float;
    xg: float; yg: float;
    xb: float; yb: float
}
type white = { xw: float; yw: float }

let print_xy name x y =
  Printf.printf "%s: x=%.3f, y=%.3f\n"
    name  x y

let parse_chromacity edid =
  let combine bits9to2 lowbits lowbitshift =
    let bits = (bits9to2 lsl 2) lor ((lowbits lsr lowbitshift) land 0x3) in
    (float bits) /. 1024. in
  let rglow = Char.code edid.[0x19]
  and bwlow = Char.code edid.[0x1A]
  and rx = Char.code edid.[0x1B] and ry = Char.code edid.[0x1C]
  and gx = Char.code edid.[0x1D] and gy = Char.code edid.[0x1E]
  and bx = Char.code edid.[0x1F] and by = Char.code edid.[0x20]
  and wx = Char.code edid.[0x21] and wy = Char.code edid.[0x22] in
  (* 10-bit accuracy *)
  let p = {
    xr = combine rx rglow 6;
    yr = combine ry rglow 4;
    xg = combine gx rglow 2;
    yg = combine gy rglow 0;
    xb = combine bx bwlow 6;
    yb = combine by bwlow 4;
  }
  and whitepoint = {
    xw = combine wx bwlow 2;
    yw = combine wy bwlow 0;
  } in
  print_xy "Red" p.xr p.yr;
  print_xy "Green" p.xg p.yg;
  print_xy "Blue" p.xb p.yb;
  print_xy "White" whitepoint.xw whitepoint.yw;
  p, whitepoint

let cp437_eolsp name s =
  Printf.printf "%s: %s\n" name s;;

let parse_descriptor edid pos =
  let desc = String.sub edid pos 18 in
  if desc.[0] <> '\x00' || desc.[1] <> '\x00' then
    (* detailed timings *)
    let horizlo = Char.code desc.[12]
    and vertlo = Char.code desc.[13] in
    let hi = Char.code desc.[14] in
    let horiz = ((hi lsl 4) land 0xf00) lor horizlo
    and vert = ((hi lsl 8) land 0xf00) lor vertlo in
    Printf.printf "%d mm x %d mm\n" horiz vert;
  else let data = String.sub desc 5 12 in
  match Char.code desc.[3] with
  | 0xFF ->
      cp437_eolsp "serial number" data
  | 0xFE ->
      cp437_eolsp "ascii data" data
  | 0xFC ->
      cp437_eolsp "monitor name" data
  | 0xFB ->
      cp437_eolsp "color point" data
  | n ->
      Printf.printf "other tag: %02x\n" n

let parse_edid edid =
  if (String.length edid mod 128 > 0) then
    invalid_arg "EDID length must be multiple of 128";
  if (String.sub edid 0 8) <> edid_header then
    invalid_arg "Bad EDID header";
  let sum = ref 0 in
  for i = 0 to 127 do
    sum := !sum + Char.code edid.[i]
  done;
  Printf.printf "sum: %d\n" !sum;
  if !sum mod 256 <> 0 then
    invalid_arg "EDID bad checksum";
  let manufacturer = parse_manufacturer edid in
  let productid = read_u16_le edid 0x0a in
  Printf.printf "manufacturer: %s\n" manufacturer;
  Printf.printf "product id %02x\n" productid;
  let edid_ver = Char.code edid.[0x12] in
  let edid_revision = Char.code edid.[0x13] in
  Printf.printf "EDID %d.%d\n" edid_ver edid_revision;
  let horiz = Char.code edid.[0x15]
  and vert = Char.code edid.[0x16] in
  (* 0 = indeterminate size (e.g. projector) *)
  Printf.printf "Physical size: %d cm x %d cm\n" horiz vert;
  let Some gamma = parse_gamma edid in
  Printf.printf "gamma: %f\n" gamma;
  let feature = Char.code edid.[0x18] in
  let display_type = (feature lsr 3) land 0x3 in
  Printf.printf "display type: %02x\n" display_type;
  let is_srgb = (feature lsr 2) = 1 in
  Printf.printf "sRGB: %b\n" is_srgb;
  let chroma = parse_chromacity edid in
  let () = parse_descriptor edid 0x36 in
  parse_descriptor edid 0x48;
  parse_descriptor edid 0x5A;
  parse_descriptor edid 0x6C;
  let extension = Char.code edid.[0x7E] in
  if (1+extension) * 128 <> String.length edid then
    invalid_arg "EDID number of extensions doesn't match size";
  ()

let load_edid name =
  let f = open_in name in
  let n = in_channel_length f in
  let edid = String.make n '\000' in
  really_input f edid 0 n;
  close_in f;
  edid

let () =
  parse_edid (load_edid "myedid");
  parse_edid (load_edid "laptopedid");
  parse_edid (load_edid "laptopedid2");
  ();;




