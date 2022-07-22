#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "gg" @@ fun c ->
  Ok [ Pkg.mllib "src/gg.mllib";
       Pkg.mllib ~api:[] "src/gg_top.mllib";
       Pkg.mllib ~api:["Gg_kit"] "src-kit/gg_kit.mllib" ~dst_dir:"kit";
       Pkg.mllib ~api:["Gg_unstable"]
         "src-unstable/gg_unstable.mllib" ~dst_dir:"unstable";
       Pkg.lib "src/gg_top_init.ml";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
(*       Pkg.test "test/benchmark"; *)
       Pkg.test "test/test"; ]
