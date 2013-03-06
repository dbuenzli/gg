(* Very early draft of Bigarray, Gg.Color, Gg.Raster and ICC profile integration *)
val parse_icc : string -> Gg.Color.profile
val write_iccv4 : Gg.Color.profile -> string

(* generic bigarray to bigarray colorspace conversion *)
val convert_ba : Gg.raster -> Gg.Color.profile ->
  Gg.Color.profile -> Gg.raster
