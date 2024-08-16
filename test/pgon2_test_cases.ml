(*---------------------------------------------------------------------------
   Copyright (c) 2022 The gg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Test polygons for Pgon2 boolean operations *)

let t000 =
  __LOC__,
  [[-0.5, 0.7; 0.5, -0.05; 1.5, 0.5]],
  [[0.0, 0.0; 0.5, 0.5; 1.0, 0.0];
   [0.15, 0.06; 0.5, 0.4; 0.85, 0.06]], None, ""

let t001 =
  __LOC__,
  [[0.0, 0.0; 1.0, 1.0; 2.0, 0.0]],
  [[0.0, 1.5; 1.0, 0.5; 2.0, 1.5];
   [0.5, 1.3; 1.0, 0.7; 1.5, 1.3]], None, ""

let t002 =
  __LOC__,
  [[0.0, 0.0; 1.0, 1.0; 2.0, 0.0];
   [0.25, 0.1; 1.0, 0.8; 1.75, 0.1]],
  [[0.0, 0.75; 1.0, -0.25; 2.0, 0.75];
   [0.5, 0.65; 1.0, -0.05; 1.5, 0.65]], None, ""

let t003 =
  __LOC__,
  [[0.0, 0.75; 1.0, -0.25; 2.0, 0.75];
   [0.5, 0.65; 1.0, -0.05; 1.5, 0.65]],
  [[0.0, 0.75; 1.0, -0.25; 2.0, 0.75];
   [0.5, 0.65; 1.0, -0.05; 1.5, 0.65]], None, "Self"

let t004 =
  __LOC__,
  [[0.0, 0.0; 0.5,  0.5; 1.0, 0.0]],
  [[0.5, 0.0; 0.5, -0.5; 1.5, 0.0]], None, ""

let t005 =
  __LOC__,
  [[0.0, 0.0; -0.5, 0.5; 0.0, 1.]],
  [[0.0, 0.5;  0.5, 0.0; 0.0, -0.5]], None, ""

let t006 =
  __LOC__,
  [[0.0, 0.0; 0.5,  0.5; 1.0, 0.0]],
  [[0.5, 0.0; 1.0,  0.5; 1.5, 0.0]], None, ""

let t007 =
  __LOC__,
  [[-1.0, 0.0; 0.0, 1.0; 1.0, 0.0; 0.0, -1.0]],
  [[-1.0 +. 0.4, 0.4; -1.0 +. 0.6, 0.6; 0.1, 0.1; 0.1, -0.3;
    0.5, -1.2; -1.2, -1.2; ]], None, "Paper fig. 3."

let t008 =
  __LOC__,
  [[115.,96.; 140.,206.; 120.,210.; 125.,250.; 80.,300.]],
  [[111.,228.; 129.,192.; 309.,282.]], None,
  "From https://github.com/w8r/martinez/issues/110. Needs resort below (?)"

let t009 =
  __LOC__,
  [[-1.0, 1.0; 1.0, -1.0; 1.0, 1.0]],
  [[0.0, 0.0; 0.0, 1.0; 1.0, 0.0]], None, "Resort below"

let t010 =
  __LOC__,
  [[0.0, 0.0; 0.0, 1.0; 1.0, 0.0]],
  [[-1.0, 1.0; 1.0, -1.0; 1.0, 1.0]], None, "Resort below."

let t011 =
  __LOC__,
  [[0x1.0a2281672d93bp+7, 0x1.90aa44afe878ep+7;
    0x1.e04705a861e1p+6, 0x1.8eb724962322fp+7;
    0x1.dc60c5df22622p+6, 0x1.90aa44afe878ep+7;
    0x1.dc60c5df22622p+6, 0x1.929d64c9adcedp+7;
    0x1.efe004cd5fdc8p+6, 0x1.9343c4d244eb7p+7;
    0x1.f27984a8df867p+6, 0x1.929d64c9adcedp+7;
    0x1.f7ac845fdeda4p+6, 0x1.929d64c9adcedp+7;
    0x1.fa46043b5e843p+6, 0x1.929d64c9adcedp+7;]],
  [[0x1.0789018bade9cp+7, 0x1.9683a4fd387aap+7;
    0x1.0595e1a70e2a5p+7, 0x1.9150a4b87f958p+7;
    0x1.044921b94e556p+7, 0x1.90aa44afe878ep+7;
    0x1.01afa1ddceab7p+7, 0x1.90aa44afe878ep+7;
    0x1.0062e1f00ed68p+7, 0x1.90aa44afe878ep+7;
    0x1.ff7903f25dd81p+6, 0x1.9150a4b87f958p+7;
    0x1.f7ac845fdeda4p+6, 0x1.91f704c116b22p+7;
    0x1.f51304845f306p+6, 0x1.91f704c116b22p+7;
    0x1.efe004cd5fdc8p+6, 0x1.9343c4d244eb7p+7;
    0x1.ed4684f1e0329p+6, 0x1.9536e4ec0a416p+7;]], None,
  "[seg_compare] needs a robust collinearity test"

let t012 =
  __LOC__,
  [[0x1.b058e04960141p+8, 0x1.72ee9b26e71a9p+8;
    0x1.ae129069505f6p+8, 0x1.74e1bb40ac708p+8;
    0x1.b0ac1044d0095p+8, 0x1.777b3b6308e31p+8;]],
  [[0x1.b1df0ffbcf5d2p+8, 0x1.762e7b51daa9cp+8;
    0x1.b152703baff3cp+8, 0x1.750e7b51daa9cp+8;
    0x1.b005b04df01edp+8, 0x1.76d4db5a71c67p+8;]], None, "Resort above"

let t013 =
  __LOC__,
  [[0x1.c864bef7bcf7dp+8, 0x1.0651298b4088p+9;
    0x1.c5cb3f1c3d4dep+8, 0x1.08eaa9ad9cfa9p+9;
    0x1.c90b1eee9ce25p+8, 0x1.08eaa9ad9cfa9p+9;
    0x1.cc4afec0fc76bp+8, 0x1.05aac982a96b6p+9;]],
  [[0x1.caab0ed7ccac8p+8, 0x1.0581318083a44p+9;
    0x1.c8b7eef32ced1p+8, 0x1.05d46184cf329p+9;
    0x1.c8118efc4d029p+8, 0x1.0651298b4088p+9;
    0x1.c8118efc4d029p+8, 0x1.06a4598f8c165p+9;
    0x1.c90b1eee9ce25p+8, 0x1.0774519a48fa2p+9]],
  None, "Resort below"

let t014 =
  __LOC__,
  [[0x1.2a77a7a131652p+8, 0x1.9f9ce5757c0bap+7;
    0x1.2c6ac785d1249p+8, 0x1.9e5025644dd25p+7;
    0x1.2cbdf7814119cp+8, 0x1.9da9c55bb6b5bp+7;
    0x1.3051074f10a37p+8, 0x1.9536e4ec0a416p+7;
    0x1.30a4374a8098bp+8, 0x1.8e10c48d8c065p+7;
    0x1.3051074f10a37p+8, 0x1.8cc4047c5dcdp+7;]],
  [[0x1.32ea872a904d5p+8, 0x1.a190058f41618p+7;
    0x1.3051074f10a37p+8, 0x1.9c5d054a887c6p+7;
    0x1.2e0ab76f00eecp+8, 0x1.9c5d054a887c6p+7;
    0x1.2c17978a612f5p+8, 0x1.9e5025644dd25p+7;]],
  None, "Divide left-right swap"

let t015 =
  __LOC__,
  [[0x1.3668caf983d9bp+9, 0x1.ae65ee38e9d73p+8;
    0x1.3545a3097bff5p+9, 0x1.b0ac3e56fabb7p+8;
    0x1.370f2af063c42p+9, 0x1.b0cbeec25ba17p+8;]],
  [[0x1.363f32fbcbdf1p+9, 0x1.aeb91e3d35658p+8;
    0x1.36bbfaf4f3cefp+9, 0x1.adbf8e3052ba9p+8;
    0x1.35a59afe13e47p+9, 0x1.aeb91e3d35658p+8]], None,
  "Correct overlap inout; [seg_compare] when collinear a segment extends \
   another (and maybe not exactly due to numerical errors."

let t016 =
  __LOC__,
  [[0x1.a83930bb71211p+8, 0x1.56a949b139bcbp+8;
    0x1.a932c0adc100cp+8, 0x1.57a2d9be1c67bp+8;
    0x1.ab25e09260c03p+8, 0x1.58ef99cf4aa0fp+8;]],
  [[0x1.a8df90b2510b8p+8, 0x1.59e929dc2d4bfp+8;
    0x1.a8df90b2510b8p+8, 0x1.57f609c267f6p+8;
    0x1.a8df90b2510b8p+8, 0x1.574fa9b9d0d96p+8;
    0x1.a83930bb71211p+8, 0x1.5602e9a8a2a01p+8;]], None,
  "[seg_compare] when collinear and horizontal prolongation."

and t017 =
  __LOC__,
  [[0x1.24f177eec21cp+8, 0x1.8eb724962322fp+7;
    0x1.249e47f35226cp+8, 0x1.8f5d849eba3f9p+7;
    0x1.249e47f35226cp+8, 0x1.90aa44afe878ep+7;]],
  [[0x1.2544a7ea32114p+8, 0x1.90aa44afe878ep+7;
    0x1.244b17f7e2319p+8, 0x1.8eb724962322fp+7;
    0x1.23ab280eb2675p+8, 0x1.8f10c48d8c065p+7;]],
  Some ([9], [5], [3; 3], [9]),
  "Two segs of one triangle meet on the edge of another \
   one but two different isect points are computed."

and t018 =
  __LOC__,
  [[0x1.02bdfdcefa888p+9, 0x1.e460a9040de78p+7;
    0x1.01ee05da62a36p+9, 0x1.e5cd69153c20cp+7;
    0x1.0147a5e382b8fp+9, 0x1.e673c91dd33d7p+7;]],
  [[0x1.01c46ddcaaa8cp+9, 0x1.e673c91dd33d7p+7;
    0x1.026acdd38a934p+9, 0x1.e3da48fb76caep+7;
    0x1.025465d1428dep+9, 0x1.e28d88ea48919p+7;]],
  None, "Right order swap in divide"

let t019 =
  __LOC__,
  [[0x1.026acdd38a934p+9, 0x1.e28d88ea48919p+7;
    0x1.02179dd81a9ep+9, 0x1.e28d88ea48919p+7;
    0x1.01ee05da62a36p+9, 0x1.e3da48fb76caep+7;
    0x1.fe02ac06e6087p+8, 0x1.e71a29266a5a1p+7;]],
  [[0x1.01713de13ab38p+9, 0x1.e527090ca5042p+7;
    0x1.026acdd38a934p+9, 0x1.e140c8d91a585p+7;
    0x1.0077adeeead3dp+9, 0x1.e673c91dd33d7p+7]],
  None, "Unassigned below"

let t020 =
  __LOC__,
  [[0x1.0882917dfdc98p+6, 0x1.73be9331a3fe6p+4;
    0x1.0882917dfdc98p+6, 0x1.7e2493bb15c8ap+4;
    0x1.060f91c6fe75bp+6, 0x1.757984b5cffcp+4;]],
  [[0x1.05c51b4be4aap+6, 0x1.7289959c240c8p+4;
    0x1.0882917dfdc98p+6, 0x1.78f193765ce38p+4;
    0x1.0882917dfdc98p+6, 0x1.70f390c723f04p+4;]],
    None, ""

let t021 =
  __LOC__,
  [[0x1.d5e1063a63395p+6, 0x1.07d5ddac8a17p+7;
    0x1.ddad85cce2371p+6, 0x1.082f7da3f2fa6p+7;
    0x1.ebf9c504205dap+6, 0x1.01095d4574bf5p+7;]],
  [[0x1.e42d4571a15fep+6, 0x1.0595fd819687dp+7;
    0x1.e04705a861e1p+6, 0x1.06e2bd92c4c11p+7;
    0x1.db1405f1628d2p+6, 0x1.07891d9b5bddcp+7;]], None,
  "Need field recompute on right overlap"

let t022 =
  __LOC__,
  [[0x1.39a8aacbe36e1p+9, 0x1.ae65ee38e9d73p+8;
    0x1.39557ad07378dp+9, 0x1.aeb91e3d35658p+8;
    0x1.38af1ad9938e6p+9, 0x1.aeb91e3d35658p+8;]],
  [[0x1.39024ad50383ap+9, 0x1.af0c4e4180f3dp+8;
    0x1.39a8aacbe36e1p+9, 0x1.ae65ee38e9d73p+8;
    0x1.39fbdac753635p+9, 0x1.ae12be349e48ep+8;
    0x1.39fbdac753635p+9, 0x1.ad6c5e2c072c3p+8;]], None, ""

let t023 =
  __LOC__,
  [[0x1.755e5b857fb4fp+7, 0x1.0504697a124ecp+9;
    0x1.7b37bb335ef35p+7, 0x1.06cdf191b1dd8p+9;
    0x1.7dd13b0ede9d3p+7, 0x1.0774519a48fa2p+9;]],
  [[0x1.7ec45af37e5cap+7, 0x1.079de99c6ec15p+9;
    0x1.7c847b211ec84p+7, 0x1.07212195fd6bdp+9;
    0x1.7bde1b2a3eddcp+7, 0x1.06f78993d7a4bp+9;
    0x1.79449b4ebf33dp+7, 0x1.06f78993d7a4bp+9;]], None, ""

let t024 =
  __LOC__,
  [[0x1.cbf7cec58c817p+8, 0x1.05aac982a96b6p+9;
    0x1.caab0ed7ccac8p+8, 0x1.05d46184cf329p+9;
    0x1.ca57dedc5cb74p+8, 0x1.05fdf986f4f9bp+9;
    0x1.ca04aee0ecc2p+8, 0x1.0651298b4088p+9;]],
  [[0x1.ca04aee0ecc2p+8, 0x1.062791891ac0ep+9;
    0x1.cafe3ed33ca1cp+8, 0x1.05aac982a96b6p+9;
    0x1.ca816eceac97p+8, 0x1.055ad177ec879p+9;]],
  None, "We miss the overlap and think that the triangle below is above."

let t025 =
  __LOC__,
  [[0x1.702b5bce80612p+7, 0x1.ab260e0df648p+8;
    0x1.5f459cbbc290ap+7, 0x1.af5f7e45cc822p+8;
    0x1.632bdc85020f8p+7, 0x1.b345be79572ep+8; ]],
  [[0x1.63d23c7be1fap+7, 0x1.b3ec1e81ee4aap+8;
    0x1.62857c8e2225p+7, 0x1.b29f5e70c0115p+8;
    0x1.62857c8e2225p+7, 0x1.b1f8fe6828f4bp+8;
    0x1.5a129d04c33cdp+7, 0x1.b1b2ae4a18107p+8;]], None, ""

let t026 =
  __LOC__,
  [[0x1.5029658fec84fp+8, 0x1.726473e60957dp+6;
    0x1.53161566dc242p+8, 0x1.707eb306b06f3p+6;
    0x1.52c2e56b6c2eep+8, 0x1.6f31f2f58235ep+6;
    0x1.536945624c196p+8, 0x1.64cbf26c106bap+6;]],
  [[0x1.526fb56ffc39ap+8, 0x1.6ab2329f9b178p+6;
    0x1.526fb56ffc39ap+8, 0x1.6de532e453fcap+6;
    0x1.536945624c196p+8, 0x1.71cb7317dea87p+6;]], None, "Overlap"

let t027 =
  __LOC__,
  [[ 0.0, 0.0; 1.0, 0.0; 1.0, 1.0; 0.0, 1.0]],
  [[ 0.25, 0.3; 0.75, 0.3; 0.75, 1.0; 0.25, 1.0]],
    None, "Overlap test, fig. 10. 2013 paper"

let t028 =
  __LOC__,
  [[ 0.0, 0.0; 1.0, 0.0; 1.0, 1.0; 0.0, 1.0]],
  [[ 0.25, 1.0; 0.75, 1.0; 0.75, 1.75; 0.25, 1.75]],
    None, "Overlap test, fig. 10. 2013 paper"

let make_test (loc, a,  b, ops, doc) =
  let pt (x, y) = Gg.P2.v x y in
  let ring ps = Gg_kit.Ring2.of_pts (List.map pt ps) in
  let pgon cs = Gg_kit.Pgon2.of_rings (List.map ring cs) in
  loc, pgon a, pgon b, ops, doc

let list = List.rev_map make_test
    [ t000; t001; t002; t003; t004; t005; t006; t007; t008; t009; t010;
      t011; t012; t013; t014; t015; t016; t017; t018; t019; t020;
      t021; t022; t023; t026; t027; t028; t025; t024]
