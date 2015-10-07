{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
let
  callPackage = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage;
in
callPackage ./hlatex.nix {
  # X = callPackage ./nix/X.nix {};
}
