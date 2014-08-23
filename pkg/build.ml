#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "gg" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/gg";
    Pkg.lib ~exts:Exts.library "src/gg_top";
    Pkg.lib "src/gg_top_init.ml";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "AUTHORS.md"; ]
