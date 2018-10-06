{ compiler ? "ghc822", doBenchmark ? false }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nix/nixpkgs-version.json);
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/${pinnedVersion.rev}.tar.gz";
    sha256 = pinnedVersion.sha256;
  };
  pkgs = import nixpkgs {
    config = {
      packageOverrides = oldPkgs: {
        glibc = pkgsUnstable.glibc;
      };
    };
  };

  haskellPackagesStable = pkgs.haskell.packages.${compiler}.override {
    overrides = haskellNew: haskellOld: rec {
      mkDerivation = args: haskellOld.mkDerivation (args // {
        enableLibraryProfiling = true;
      });
    };
  };

  # We want to be able to pull some packages from _the future_!
  unstablePinnedVersion = builtins.fromJSON (builtins.readFile ./nix/nixpkgs-unstable-version.json);
  nixpkgsUnstable = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/${unstablePinnedVersion.rev}.tar.gz";
    sha256 = unstablePinnedVersion.sha256;
  };
  pkgsUnstable = import nixpkgsUnstable {};

  f = import ./default.nix;

  # We're using pkgsUnstable as it has ecstasy 2.0. We can replace this with nixos-18.09 when
  # it releases.
  haskellPackages = pkgsUnstable.haskell.packages.${compiler}.override {
    overrides = haskellNew: haskellOld: rec {
      # OpenGL segfaults when we try to haddock it. It also fails to link when we use
      # a version of OpenGL from a nixpkgs later then our NixOS install. It seems to be unable
      # to find certain OpenGL symbols.
      #
      # To work around this we use the stable versions of the OpenGL libraries.
      GLUT = pkgs.haskell.lib.dontHaddock haskellPackagesStable.GLUT;
      OpenGL = pkgs.haskell.lib.dontHaddock haskellPackagesStable.OpenGL;
      OpenGLRaw = pkgs.haskell.lib.dontHaddock haskellPackagesStable.OpenGLRaw;

      # Since we're using some stable stuff, sometimes we need to force other libraries
      # to fall back to their stable version to prevent cabal from complaining.
      kan-extensions = haskellPackagesStable.kan-extensions;
      lens = haskellPackagesStable.lens;
      sdl2 = haskellPackagesStable.sdl2;
      sdl2-ttf = haskellPackagesStable.sdl2-ttf;
      hpack = haskellPackagesStable.hpack;

      # these tests take _forever_
      linear = pkgsUnstable.haskell.lib.dontCheck haskellPackagesStable.linear;
      happy = pkgsUnstable.haskell.lib.dontCheck haskellOld.happy;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
in
  if pkgs.lib.inNixShell then drv.env else drv
