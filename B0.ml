open B0_kit.V000

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"

let gg = B0_ocaml.libname "gg"
let gg_top = B0_ocaml.libname "gg.top"
let gg_kit = B0_ocaml.libname "gg.kit"
let gg_unstable = B0_ocaml.libname "gg.unstable"

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

let gg_kit_lib =
  let srcs = Fpath.[ `Dir (v "src-kit") ] in
  let requires = [gg] in
  let doc = "The gg kit library" in
  B0_ocaml.lib gg_kit ~doc ~srcs ~requires

let gg_unstable_lib =
  let srcs = Fpath.[ `Dir (v "src-unstable") ] in
  let requires = [gg] in
  let doc = "The gg unstable library" in
  B0_ocaml.lib gg_unstable ~doc ~srcs ~requires

(* Tests *)

let test_exe ?(requires = []) src ~doc =
  let src = Fpath.v src in
  let srcs = Fpath.[`File src] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = gg :: requires in
  B0_ocaml.exe (Fpath.basename ~no_ext:true src) ~srcs ~doc ~meta ~requires

let test =
  let srcs =
    Fpath.[`File (v "test/test.ml"); `File (v "test/checkm.ml");
           `File (v "test/checkm.mli")]
  in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [gg; str] in
  let doc = "Gg test suite" in
  B0_ocaml.exe "test" ~srcs ~doc ~meta ~requires


(* N.B. Unless vg is in the build universe, those tests with vg needs to be
   build with `-x gg` otherwise we get inconsistent assumptions. See the
   pgon-viz pack. *)

let vg = B0_ocaml.libname "vg"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"
let vg_pdf = B0_ocaml.libname "vg.pdf"
let brr = B0_ocaml.libname "brr"

let pgon_debug =
  let srcs =
    Fpath.[`Dir (v "src-unstable");
           `File (v "test/pgon_cases.ml");
           `File (v "test/pgon_debug.ml");]
  in
  let requires = [gg; vg; vg_htmlc; brr] in
  let assets_root = Fpath.v "test" in
  let meta =
    let comp_mode = `Separate in
    B0_jsoo.meta ~requires ~assets_root ~comp_mode ~source_map:(Some `Inline) ()
  in
  B0_jsoo.web "pgon_debug" ~doc:"Polygon boolean ops step debugger" ~srcs ~meta

let pgon_tests =
  let srcs =
    Fpath.[`Dir (v "src-unstable");
           `File (v "test/pgon_cases.ml");
           `File (v "test/pgon_tests.ml");]
  in
  let requires = [b0_std; gg; vg; vg_pdf] in
  let meta = B0_meta.(empty |> tag test) in
  let doc = "Polygon boolean op tests" in
  B0_ocaml.exe "pgon_tests" ~srcs ~doc ~meta ~requires

(* Packs *)

let pgon_test_pack =
  (* We use a locked pack so that we compile against the installed gg
     otherwise we compile gg and we get inconsistent assumptions with
     installed vg. *)
  let meta = B0_meta.(empty |> tag test) in
  B0_pack.v "pgon-test" ~doc:"Polygon visual testing" ~meta ~locked:true @@
  [pgon_debug; pgon_tests]

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
        "brr", {|test|};
        "vg", {|test|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"gg package" ~meta ~locked:true @@
  [gg_lib; gg_kit_lib; gg_top_lib; gg_unstable_lib; test]
