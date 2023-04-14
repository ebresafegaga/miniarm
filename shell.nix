let 
  pkgs = import <nixpkgs> {};
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    git
    bash
    opam
    ocamlPackages.ppx_inline_test
    ocamlPackages.expect_test_helpers_core
    ocamlPackages.alcotest
    ocamlPackages.ocaml
    ocamlPackages.dune_3
    ocamlPackages.menhir
    ocamlPackages.findlib
    ocamlPackages.ppxlib
    ocamlPackages.ppx_deriving
  ];
}