{ pkgs ? import ./nix/pkgs.nix {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc884
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.haskellPackages.hpack
    pkgs.zlib
    pkgs.openssl
    pkgs.postgresql
    pkgs.ghcid
    pkgs.haskellPackages.hoogle
  ];
}
