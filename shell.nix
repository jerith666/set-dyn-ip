{ sources ? import nix/sources.nix }:

let
  nixpkgs = import sources.nixpkgs { };
in
with nixpkgs;

let

  inherit (nixpkgs) pkgs;
  haskellPkgs = pkgs.haskellPackages;
  ghc = haskellPkgs.ghcWithPackages (ps: with ps; [
    amazonka
    amazonka-core
    amazonka-route53
    base
    lens
    network
  ]);

in

pkgs.stdenv.mkDerivation {
  name = "set-dyn-ip-env-0";
  buildInputs = with pkgs; with haskellPkgs; [
    ghc

    cabal-install
    ghcide
    haskell-language-server
    haskell-dap
    ghci-dap
    haskell-debug-adapter

    ormolu

    vscodium

    niv
    nixpkgs-fmt
    nixd
  ];
}
