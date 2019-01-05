{ compiler ? "ghc844", doBenchmark ? false }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nix/nixpkgs-version.json);
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/${pinnedVersion.rev}.tar.gz";
    sha256 = pinnedVersion.sha256;
  };
  pkgs = import nixpkgs {};

  crossyToad = import ./default.nix;

  # We're using pkgsUnstable as it has ecstasy 2.0. We can replace this with nixos-18.09 when
  # it releases.
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = haskellNew: haskellOld: rec {
      ghc = haskellOld.ghc // { withPackages = haskellOld.ghc.withHoogle; };
      ghcWithPackages = haskellNew.ghc.withPackages;

      # sdl2 tests fail with an error related to an XDG environment variable
      sdl2 = pkgs.haskell.lib.dontCheck haskellOld.sdl2;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage crossyToad {});
in
  if pkgs.lib.inNixShell then drv.env else drv
