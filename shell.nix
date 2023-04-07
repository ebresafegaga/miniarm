with import <nixpkgs> {};

pkgs.mkShell {
  buildInputs = with pkgs; [
    git
    bash
    opam
    ocamlPackages.ocaml
    ocamlPackages.dune_3
    ocamlPackages.menhir
    ocamlPackages.findlib
    ocamlPackages.ppxlib
    ocamlPackages.ppx_deriving
  ];
}