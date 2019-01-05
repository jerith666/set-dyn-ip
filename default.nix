{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.haskellPackages.callPackage ./set-dyn-ip.nix { }
