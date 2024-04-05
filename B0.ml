open B0_kit.V000

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"

let gg = B0_ocaml.libname "gg"
let gg_top = B0_ocaml.libname "gg.top"
let gg_kit = B0_ocaml.libname "gg.kit"

let str = B0_ocaml.libname "str"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

(* Libraries *)

let gg_lib =
  let srcs = [ `File ~/"src/gg.mli"; `File ~/"src/gg.ml" ] in
  B0_ocaml.lib gg ~doc:"The gg library" ~srcs

let gg_top_lib =
  let srcs = [ `File ~/"src/gg_top.ml" ] in
  let requires = [compiler_libs_toplevel] in
  B0_ocaml.lib gg_top ~doc:"The gg toplevel support library" ~srcs ~requires

let gg_kit_lib =
  let srcs = [ `Dir ~/"src/kit" ] in
  B0_ocaml.lib gg_kit ~doc:"The gg kit library" ~srcs ~requires:[gg]

(* Tests *)

let test_exe ?(requires = []) src ~doc =
  let srcs = [`File src] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = gg :: requires in
  B0_ocaml.exe (Fpath.basename ~strip_ext:true src) ~srcs ~doc ~meta ~requires

let test =
  let doc = "Gg test suite" in
  let srcs =
    [ `File ~/"test/test.ml"; `File ~/"test/checkm.ml";
      `File ~/"test/checkm.mli" ]
  in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [gg; str] in
  B0_ocaml.exe "test" ~srcs ~doc ~meta ~requires

let test_color_schemes =
  let doc = "Color scheme tests" in
  test_exe ~/"test/test_color_schemes.ml" ~doc ~requires:[gg_kit]

(* N.B. Unless vg is in the build, those tests with vg needs to be
   build with `-x gg` otherwise we get inconsistent assumptions. See
   the also the pgon2_bool_tests pack. *)

let vg = B0_ocaml.libname "vg"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"
let vg_pdf = B0_ocaml.libname "vg.pdf"
let brr = B0_ocaml.libname "brr"

let pgon2_bool_steps =
  let doc = "Pgon2 boolean operations step debugger" in
  let srcs =
    [`Dir ~/"src_kit";
     `File ~/"test/pgon2_test_cases.ml";
     `File ~/"test/pgon2_bool_steps.ml";]
  in
  let requires = [gg; vg; vg_htmlc; brr] in
  let assets_root = ~/"test" in
  let meta =
    B0_meta.empty
    |> ~~ B0_jsoo.compilation_mode `Separate
    |> ~~ B0_jsoo.source_map (Some `Inline)
  in
  B0_jsoo.html_page "pgon2_bool_steps" ~assets_root ~requires ~doc ~srcs ~meta

let pgon2_bool_tests =
  let doc = "Pgon2 boolean operations tests" in
  let srcs =
    [ `Dir ~/"src_kit";
      `File ~/"test/pgon2_test_cases.ml";
      `File ~/"test/pgon2_bool_tests.ml";]
  in
  let requires = [b0_std; gg; vg; vg_pdf] in
  let meta = B0_meta.(empty |> tag test) in
  B0_ocaml.exe "pgon2_bool_tests" ~srcs ~doc ~meta ~requires

let viz_orient =
  let doc = "Orientation predicate visualization" in
  let srcs = [ `File ~/"test/orient_p2.ml" ] in
  let requires = [gg; brr] in
  let meta =
    B0_meta.empty
    |> ~~ B0_jsoo.compilation_mode `Separate
    |> ~~ B0_jsoo.source_map (Some `Inline)
  in
  B0_jsoo.html_page "orient_p2" ~requires ~doc ~srcs ~meta

let color_schemes =
  let doc = "Color schemes visualization"in
  let srcs = [`File ~/"test/color_schemes.ml"] in
  let requires = [gg; gg_kit; brr; vg; vg_htmlc] in
  let meta =
    B0_meta.empty
    |> ~~ B0_jsoo.compilation_mode `Separate
    |> ~~ B0_jsoo.source_map (Some `Inline)
  in
  B0_jsoo.html_page "color_schemes" ~requires ~doc ~srcs ~meta

(* Packs *)

let pgon_test_pack =
  (* We use a locked pack so that we compile against the installed gg
     otherwise we compile gg and we get inconsistent assumptions with
     installed vg. *)
  let meta = B0_meta.(empty |> tag test) in
  let doc = "Pgon2 boolean operations visual testing" in
  B0_pack.make "pgon2_bool_tests" ~doc ~meta ~locked:true @@
  [pgon2_bool_tests; pgon2_bool_steps]

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The gg programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/gg"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/gg/doc/"
    |> ~~ B0_meta.licenses ["ISC"; "Apache-2.0"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/gg.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/gg/issues"
    |> ~~ B0_meta.description_tags
      ["matrix"; "vector"; "color"; "data-structure"; "graphics";
       "org:erratique"]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "brr", {|with-test|};
        "vg", {|with-test|};
      ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"gg package" ~meta ~locked:true @@
  [gg_lib; gg_kit_lib; gg_top_lib; test; viz_orient]
