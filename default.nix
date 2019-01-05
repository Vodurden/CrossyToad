{ compiler ? "ghc844" }:

let
  pkgs = import ./nix/nixpkgs.nix {};

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = haskellNew: haskellOld: rec {
      ghc = haskellOld.ghc // { withPackages = haskellOld.ghc.withHoogle; };
      ghcWithPackages = haskellNew.ghc.withPackages;

      # sdl2 tests fail with an error related to an XDG environment variable
      sdl2 = pkgs.haskell.lib.dontCheck haskellOld.sdl2;
    };
  };

  crossyToad = haskellPackages.callPackage ./crossy-toad.nix {};
in
  crossyToad
