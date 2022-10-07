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
  let srcs = Fpath.[ `File (v "src/gg.mli"); `File (v "src/gg.ml") ] in
  let requires = [] in
  B0_ocaml.lib gg ~doc:"The gg library" ~srcs ~requires

let gg_top_lib =
  let srcs = Fpath.[ `File (v "src/gg_top.ml") ] in
  let requires = [compiler_libs_toplevel] in
  let doc = "The gg toplevel support library" in
  B0_ocaml.lib gg_top ~doc ~srcs ~requires

let gg_kit_lib =
  let srcs = Fpath.[ `Dir (v "src_kit") ] in
  let requires = [gg] in
  let doc = "The gg kit library" in
  B0_ocaml.lib gg_kit ~doc ~srcs ~requires

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


let test_color_schemes =
  let doc = "Color scheme tests" in
  test_exe "test/test_color_schemes.ml" ~doc ~requires:[gg_kit]

(* N.B. Unless vg is in the build universe, those tests with vg needs
   to be build with `-x gg` otherwise we get inconsistent
   assumptions. See the also the pgon2_bool_tests pack. *)

let vg = B0_ocaml.libname "vg"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"
let vg_pdf = B0_ocaml.libname "vg.pdf"
let brr = B0_ocaml.libname "brr"

let pgon2_bool_steps =
  let srcs =
    Fpath.[`Dir (v "src_kit");
           `File (v "test/pgon2_test_cases.ml");
           `File (v "test/pgon2_bool_steps.ml");]
  in
  let requires = [gg; vg; vg_htmlc; brr] in
  let assets_root = Fpath.v "test" in
  let meta =
    let comp_mode = `Separate in
    B0_jsoo.meta ~requires ~assets_root ~comp_mode ~source_map:(Some `Inline) ()
  in
  let doc = "Pgon2 boolean operations step debugger" in
  B0_jsoo.web "pgon2_bool_steps" ~doc ~srcs ~meta

let pgon2_bool_tests =
  let srcs =
    Fpath.[`Dir (v "src_kit");
           `File (v "test/pgon2_test_cases.ml");
           `File (v "test/pgon2_bool_tests.ml");]
  in
  let requires = [b0_std; gg; vg; vg_pdf] in
  let meta = B0_meta.(empty |> tag test) in
  let doc = "Pgon2 boolean operations tests" in
  B0_ocaml.exe "pgon2_bool_tests" ~srcs ~doc ~meta ~requires

let viz_orient =
  let srcs = Fpath.[`File (v "test/orient_p2.ml")] in
  let requires = [gg; brr] in
  let meta =
    let comp_mode = `Separate in
    B0_jsoo.meta ~requires ~comp_mode ~source_map:(Some `Inline) ()
  in
  let doc = "Orientation predicate visualization"in
  B0_jsoo.web "orient_p2" ~doc ~srcs ~meta

let color_schemes =
  let srcs = Fpath.[`File (v "test/color_schemes.ml")] in
  let requires = [gg; gg_kit; brr; vg; vg_htmlc] in
  let meta =
    let comp_mode = `Separate in
    B0_jsoo.meta ~requires ~comp_mode ~source_map:(Some `Inline) ()
  in
  let doc = "Color schemes visualization"in
  B0_jsoo.web "color_schemes" ~doc ~srcs ~meta

(* Packs *)

let pgon_test_pack =
  (* We use a locked pack so that we compile against the installed gg
     otherwise we compile gg and we get inconsistent assumptions with
     installed vg. *)
  let meta = B0_meta.(empty |> tag test) in
  let doc = "Pgon2 boolean operations visual testing" in
  B0_pack.v "pgon2_bool_tests" ~doc ~meta ~locked:true @@
  [pgon2_bool_tests; pgon2_bool_steps]

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The gg programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/gg"
    |> add online_doc "https://erratique.ch/software/gg/doc/"
    |> add licenses ["ISC"; "Apache-2.0"]
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
        "brr", {|with-test|};
        "vg", {|with-test|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"gg package" ~meta ~locked:true @@
  [gg_lib; gg_kit_lib; gg_top_lib; test; viz_orient]
