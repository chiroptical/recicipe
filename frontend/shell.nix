{ pkgs ? import ../nix/pkgs.nix {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.spago
    pkgs.nodePackages.parcel-bundler
    pkgs.nodePackages.http-server
    pkgs.nodePackages.npm
    pkgs.nodePackages.purty
  ];
}
