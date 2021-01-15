{ pkgs ? import ../nix/pkgs.nix {} }:

let
  haskellPackages = (hp: with hp; [
    hspec
    hspec-megaparsec
    megaparsec
    parser-combinators
    units
  ]);

  ghc = pkgs.haskell.packages.ghc884.ghcWithHoogle haskellPackages;

in
  pkgs.mkShell {
    buildInputs = [
      ghc
      pkgs.cabal-install
      pkgs.cabal2nix
      pkgs.ghcid
      pkgs.haskellPackages.hpack
      pkgs.hlint
    ];
  }
