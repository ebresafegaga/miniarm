with import <nixpkgs> {};

let
  ocamlPackages = pkgs.recurseIntoAttrs pkgs.ocaml-ng.ocamlPackages_latest;
in
  pkgs.mkShell {
     buildInputs = with pkgs; [
      dune_3
    ] ++ ( with ocamlPackages;
    [
      ocaml
      findlib
      utop
      merlin
      menhir
      ocp-indent
    ]);
  }
