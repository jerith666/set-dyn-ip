{ sources ? import nix/sources.nix }:

let
  nixpkgs = import sources.nixpkgs { };
in
with nixpkgs;

nixpkgs.pkgs.haskellPackages.callPackage ./set-dyn-ip.nix { }
