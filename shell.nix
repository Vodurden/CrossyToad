{ compiler ? "ghc822", doBenchmark ? false }:

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
      # sdl2 tests fail with an error related to an XDG environment variable
      sdl2 = pkgs.haskell.lib.dontCheck haskellOld.sdl2;

      # OpenGL segfaults when we try to haddock it. It also fails to link when we use
      # a version of OpenGL from a nixpkgs later then our NixOS install. It seems to be unable
      # to find certain OpenGL symbols.
      #
      # To work around this we use the stable versions of the OpenGL libraries.
      # GLUT = pkgs.haskell.lib.dontHaddock haskellPackagesStable.GLUT;
      # OpenGL = pkgs.haskell.lib.dontHaddock haskellPackagesStable.OpenGL;
      # OpenGLRaw = pkgs.haskell.lib.dontHaddock haskellPackagesStable.OpenGLRaw;

      # Since we're using some stable stuff, sometimes we need to force other libraries
      # to fall back to their stable version to prevent cabal from complaining.
      # kan-extensions = haskellPackagesStable.kan-extensions;
      # lens = haskellPackagesStable.lens;
      # sdl2 = haskellPackagesStable.sdl2;
      # sdl2-ttf = haskellPackagesStable.sdl2-ttf;
      # hpack = haskellPackagesStable.hpack;

      # these tests take _forever_
      # linear = pkgsUnstable.haskell.lib.dontCheck haskellOld.linear;
      # happy = pkgsUnstable.haskell.lib.dontCheck haskellOld.happy;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage crossyToad {});
in
  if pkgs.lib.inNixShell then drv.env else drv
