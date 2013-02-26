(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.     
   Distributed under a BSD license, see ../../LICENSE.                  
  ---------------------------------------------------------------------------*)

let p = [| (* Ken Perlin's random permutation of 0..255 *)
  151; 160; 137; 91; 90; 15; 131; 13; 201; 95; 96; 53; 194; 233; 7; 225; 140; 
  36; 103; 30; 69; 142; 8; 99; 37; 240; 21; 10; 23; 190;  6; 148; 247; 120; 
  234; 75; 0; 26; 197; 62; 94; 252; 219; 203; 117; 35; 11; 32; 57; 177; 33; 
  88; 237; 149; 56; 87; 174; 20; 125; 136; 171; 168;  68; 175; 74; 165; 71; 
  134; 139; 48; 27; 166; 77; 146; 158; 231; 83; 111; 229; 122; 60; 211; 133; 
  230; 220; 105; 92; 41; 55; 46; 245; 40; 244; 102; 143; 54;  65; 25; 63; 161;
  1; 216; 80; 73; 209; 76; 132; 187; 208;  89; 18; 169; 200; 196; 135; 130; 
  116; 188; 159; 86; 164; 100; 109; 198; 173; 186;  3; 64; 52; 217; 226; 250; 
  124; 123; 5; 202; 38; 147; 118; 126; 255; 82; 85; 212; 207; 206; 59; 227; 
  47; 16; 58; 17; 182; 189; 28; 42; 223; 183; 170; 213; 119; 248; 152; 2; 44; 
  154; 163;  70; 221; 153; 101; 155; 167;  43; 172; 9; 129; 22; 39; 253;  19; 
  98; 108; 110; 79; 113; 224; 232; 178; 185;  112; 104; 218; 246; 97; 228; 
  251; 34; 242; 193; 238; 210; 144; 12; 191; 179; 162; 241;  81; 51; 145; 235; 
  249; 14; 239; 107; 49; 192; 214;  31; 181; 199; 106; 157; 184;  84; 204; 176;
  115; 121; 50; 45; 127;  4; 150; 254; 138; 236; 205; 93; 222; 114; 67; 29; 24;
  72; 243; 141; 128; 195; 78; 66; 215; 61; 156; 180;
  (* Start again *)
  151; 160; 137; 91; 90; 15; 131; 13; 201; 95; 96; 53; 194; 233; 7; 225; 140; 
  36; 103; 30; 69; 142; 8; 99; 37; 240; 21; 10; 23; 190;  6; 148; 247; 120; 
  234; 75; 0; 26; 197; 62; 94; 252; 219; 203; 117; 35; 11; 32; 57; 177; 33; 
  88; 237; 149; 56; 87; 174; 20; 125; 136; 171; 168;  68; 175; 74; 165; 71; 
  134; 139; 48; 27; 166; 77; 146; 158; 231; 83; 111; 229; 122; 60; 211; 133; 
  230; 220; 105; 92; 41; 55; 46; 245; 40; 244; 102; 143; 54;  65; 25; 63; 161;
  1; 216; 80; 73; 209; 76; 132; 187; 208;  89; 18; 169; 200; 196; 135; 130; 
  116; 188; 159; 86; 164; 100; 109; 198; 173; 186;  3; 64; 52; 217; 226; 250; 
  124; 123; 5; 202; 38; 147; 118; 126; 255; 82; 85; 212; 207; 206; 59; 227; 
  47; 16; 58; 17; 182; 189; 28; 42; 223; 183; 170; 213; 119; 248; 152; 2; 44; 
  154; 163;  70; 221; 153; 101; 155; 167;  43; 172; 9; 129; 22; 39; 253;  19; 
  98; 108; 110; 79; 113; 224; 232; 178; 185;  112; 104; 218; 246; 97; 228; 
  251; 34; 242; 193; 238; 210; 144; 12; 191; 179; 162; 241;  81; 51; 145; 235; 
  249; 14; 239; 107; 49; 192; 214;  31; 181; 199; 106; 157; 184;  84; 204; 176;
  115; 121; 50; 45; 127;  4; 150; 254; 138; 236; 205; 93; 222; 114; 67; 29; 24;
  72; 243; 141; 128; 195; 78; 66; 215; 61; 156; 180;
|]

let iof = int_of_float
let foi = float_of_int

let grad hash x y z = 
  let h = hash land 0b1111 in
  let u = if h < 8 then x else y in
  let v = if h < 4 then y else if h = 12 || h = 14 then x else z in
  (if (h land 1) = 0 then u else -. u) +. (if (h land 2) = 0 then v else -. v)

let simplex1 x =
  let scale = 1.0 in
  let hash x = p.(x) in
  let grad h x = if (h land 1) = 0 then x else -.x in
  let n h x = 
    let t = 1.0 -. (x *. x) in
    if t < 0. then 0. else
    let t = t *. t in
    t *. t *. (grad h x)
  in
  let i = floor x in
  let x0 = x -. i in
  let i1 = i +. 1. in
  let x1 = x0 -. 1.0 in
  let h0 = hash ((iof i) land 255) in
  let n0 = n h0 x0 in
  let h1 = hash ((iof i1) land 255) in
  let n1 = n h1 x1 in
  scale *. (n0 +. n1)



(* The transformations peformed are 
   | (1 + s2)  s2      |  | (1 - t2)  -t2      |
   | s2       (1 + s2) |  | -t2       (1 - t2) |

   I map (1,1) of the simplicial grid to 1.1. 
*)

let s2 = 0.5 *. ((sqrt 3.0) -. 1.0) 
let t2 = (3.0 -. (sqrt 3.0)) /. 6.0
let offset2 = -1.0 +. 2. *. t2


(*
let yt. = 2 /. sqrt 3.

let ssimplex2 x y = 
  let hash i j = p.(i + p.(j)) in
  let grad hash x y = failwith "unimplmented"
  let noise h x y = failwith "unimplemented"
  let x0 = floor (x) in
  let y0 = floor (y *. yt) in
*)

let simplex2 x y =
  let scale = 50.0 in
  let hash i j = p.(i + p.(j)) in
  let grad hash x y = 
    let h = hash mod 0b111 in
    let u, v = match h with
    | 0 -> x, 0.
    | 1 -> -.x, 0.
    | 2 -> y, 0.
    | 3 -> -.y, 0.
    | 4 -> x, y
    | 5 -> -.x, y
    | 6 -> x, -.y
    | 7 -> -.y, -.y
    | _ -> assert false
(*    let u = if h < 4 then x else y in
    let v = if h < 2 then y else if h = 6 || h = 7 then 0. else x  in
    (if (h land 1) = 0 then u else -. u) +. (if (h land 2) = 0 then v else -. v)
*)
   in u +. v 
  in
  let n h x y =
    let t = 0.5 -. (x *. x) -. (y *. y) in
    if t < 0. then 0. else
    let t = t *. t in
    t *. t *. (grad h x y)
  in
  let strans = (x +. y) *. s2 in
  let i = floor (x +. strans) in
  let j = floor (y +. strans) in
  let t = (i +. j) *. t2 in
  let xo = i -. t in
  let yo = j -. t in
  let x0 = x -. xo in
  let y0 = y -. yo in
  let i1, j1 = if x0 > y0 then 1., 0. else 0., 1. in
  let x1 = x0 -. i1 +. offset2 in
  let y1 = y0 -. j1 +. offset2 in
  let x2 = x0 +. offset2 in
  let y2 = y0 +. offset2 in
  let i = (iof i) land 255 in
  let j = (iof j) land 255 in
  let n0 = n (hash i j) x0 y0 in
  let n1 = n (hash (i + (iof i1)) (j + (iof j1))) x1 y1 in
  let n2 = n (hash (i + 1) (j + 1)) x2 y2 in
  scale *. (n0 +. n1 +. n2)


let one_div_3 = 1. /. 3.
let one_div_6 = 1. /. 6.

let simplex3 x y z =
  let scale = 32. in
  let hash i j k = (p.(i + p.(j + p.(k)))) in
  let grad hash x y z = 
    let h = hash land 0b1111 in
    let u = if h < 8 then x else y in
    let v = if h < 4 then y else if h = 12 || h = 14 then x else z in
    (if (h land 1) = 0 then u else -. u) +. (if (h land 2) = 0 then v else -. v)
  in
  let n h x y z = 
    let t = 0.6 -. (x *. x) -. (y *. y) -. (z *. z) in
    if t < 0. then 0. else
    let t = t *. t in
    t *. t *. (grad h x y z)
  in
  let s = (x +. y +. z) *. one_div_3 in
  let i = floor (x +. s) in
  let j = floor (y +. s) in
  let k = floor (z +. s) in
  let t = (i +. j +. k) *. one_div_6 in
  let xi = i -. t in
  let yi = j -. t in
  let zi = k -. t in
  let x0 = x -. xi in
  let y0 = y -. yi in
  let z0 = z -. zi in
  let i1, j1, k1, i2, j2, k2 = 
    if x0 >= y0 then 
      if y0 >= z0 then 1., 0., 0., 1., 1., 0. else 
      if x0 >= z0 then 1., 0., 0., 1., 0., 1. else
      0., 0., 1., 1., 0., 1.
    else
      if y0 < z0 then 0., 0., 1., 0., 1., 1. else
      if x0 < z0 then 0., 1., 0., 0., 1., 1. else
      0., 1., 0., 1., 1., 0.
  in
  let x1 = x0 -. i1 +. one_div_6 in
  let y1 = y0 -. j1 +. one_div_6 in
  let z1 = z0 -. k1 +. one_div_6 in
  let x2 = x0 -. i2 +. one_div_3 in
  let y2 = y0 -. j2 +. one_div_3 in
  let z2 = z0 -. k2 +. one_div_3 in
  let x3 = x0 -. 0.5 in
  let y3 = y0 -. 0.5 in
  let z3 = z0 -. 0.5 in
  let i = (iof i) land 255 in
  let j = (iof j) land 255 in
  let k = (iof k) land 255 in
  let h0 = hash i j k in 
  let n0 = n h0 x0 y0 z0 in
  let h1 = hash (i + (iof i1)) (j + (iof j1)) (k + (iof k1)) in
  let n1 = n h1 x1 y1 z1 in
  let h2 = hash (i + (iof i2)) (j + (iof j2)) (k + (iof k2)) in
  let n2 = n h2 x2 y2 z2 in
  let h3 = hash (i + 1) (j + 1) (k + 1) in
  let n3 = n h3 x3 y3 z3 in
  scale *. (n0 +. n1 +. n2 +. n3) 


let simplex4 x y z w = failwith "TODO"


let turbulence2 x y freq = 
  let noise = simplex2 in
  let t = ref 0. in
  let freq = ref freq in
  while (!freq >= 1.) do
    t := !t +. ((abs_float (noise (!freq *. x) (!freq *. y))) /. !freq);
    freq := !freq *. 0.5;
  done;
  !t


let sum3 x y z freq = 
  let noise = simplex3 in
  let t = ref 0. in
  let freq = ref freq in
  while (!freq >= 1.) do
    t := !t +. (noise (!freq *. x) (!freq *. y) (!freq *. z));
    freq := !freq *. 0.5;
  done;
  !t
  

let turbulence3 x y z freq = 
  let noise = simplex3 in
  let t = ref 0. in
  let freq = ref freq in
  while (!freq >= 1.) do
    t := !t +. 
	((abs_float (noise (!freq *. x) (!freq *. y) (!freq *. z))) /. !freq);
    freq := !freq *. 0.5;
  done;
  !t


(*
module V = struct
  let simplex1 = simplex1
  let simplex2 v = simplex2 (V2.x v) (V2.y v)
  let simplex3 v = simplex3 (V3.x v) (V3.y v) (V3.z v)
  let simplex4 v = simplex4 (V4.x v) (V4.y v) (V4.z v) (V4.w v)

  let olano1 = olano1
  let olano2 v = olano2 (V2.x v) (V2.y v)
  let olano3 v = olano3 (V3.x v) (V3.y v) (V3.z v)
  let olano4 v = olano4 (V4.x v) (V4.y v) (V4.z v) (V4.w v)
end
*)


