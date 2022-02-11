open B0_kit.V000
open B00_std

(* OCaml library names *)

let gg = B0_ocaml.libname "gg"
let gg_top = B0_ocaml.libname "gg.top"

let str = B0_ocaml.libname "str"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

(* Libraries *)

let gg_lib =
  let srcs = Fpath.[ `File (v "src/gg.mli"); `File (v "src/gg.ml") ] in
  let requires = [] in
  B0_ocaml.lib gg ~doc:"The gg library" ~srcs ~requires

let gg_top_lib =
  let srcs = Fpath.[ `File (v "src/gg_top.ml") ] in
  let requires = [compiler_libs_toplevel] in
  let doc = "The gg toplevel support library" in
  B0_ocaml.lib gg_top ~doc ~srcs ~requires

(* Tests *)

let test_exe ?(requires = []) src ~doc =
  let src = Fpath.v src in
  let srcs = Fpath.[`File src] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = gg :: requires in
  B0_ocaml.exe (Fpath.basename ~no_ext:true src) ~srcs ~doc ~meta ~requires

let test =
  let srcs = Fpath.[`File (v "test/test.ml");
                    `File (v "test/checkm.ml");
                    `File (v "test/checkm.mli")]
  in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [gg; str] in
  let doc = "Gg test suite" in
  B0_ocaml.exe "test" ~srcs ~doc ~meta ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The gg programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/gg"
    |> add online_doc "https://erratique.ch/software/gg/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/gg.git"
    |> add issues "https://github.com/dbuenzli/gg/issues"
    |> add description_tags
      ["matrix"; "vector"; "color"; "data-structure"; "graphics";
       "org:erratique"]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"gg package" ~meta ~locked:true @@
  B0_unit.list ()
