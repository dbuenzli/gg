#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "gg" @@ fun c ->
  Ok [ Pkg.mllib "src/gg.mllib";
       Pkg.mllib ~api:[] "src/top/gg_top.mllib" ~dst_dir:"top";
       Pkg.lib "src/top/gg_top_init.ml" ~dst:"top/gg_top_init.ml";
       Pkg.mllib "src/kit/gg_kit.mllib" ~dst_dir:"kit";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld"; ]
