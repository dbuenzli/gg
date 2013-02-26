
type t = V4.t

let make ~c ~r = V4.of_v3 c ~w:r
let of_pts c pt = V4.of_v3 c ~w:(V3.norm (V3.sub pt c))
let c s = s
let r s = s.(3)
let bound_pts pts = failwith "unimplemented"
let bound_vols l = failwith "unimplemented"
let itest s s' = failwith "unimplementd"
let itest_frust s f = failwith "unimplmenetnd"
let itest_pt s f = failwith "unimplemted"
let compare = Pervasives.compare
let print fmt s = 
  Format.fprintf fmt "@[<hov 1>(c =@ %a; r =@ %F)@]" V3.print s s.(3)

let to_string l = 
  Format.fprintf Format.str_formatter "%a" print l; 
  Format.flush_str_formatter ()
