{ pkgs ? (import <nixpkgs> {}) }:

pkgs.stdenv.mkDerivation {
  name = "hconstraint";
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
       ansi-wl-pprint

       containers
       fingertree

       mtl
     ]))
  ];
}
