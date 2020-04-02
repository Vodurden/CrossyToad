# let
#   sources = import ./nix/sources.nix;
#   pkgs = import sources.nixpkgs {};

#   haskellPackages = pkgs.haskellPackages.override {
#     overrides = haskellNew: haskellOld: rec {
#       ghc = haskellOld.ghc // { withPackages = haskellOld.ghc.withHoogle; };
#       ghcWithPackages = haskellNew.ghc.withPackages;

#       # sdl2 tests fail with an error related to an XDG environment variable
#       sdl2 = pkgs.haskell.lib.dontCheck haskellOld.sdl2;
#     };
#   };

#   crossyToad = haskellPackages.callPackage ./crossy-toad.nix {};
# in
#   crossyToad

  # # Fetch the latest haskell.nix and import its default.nix
  # haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  # # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # # you will be more likely to get cache hits when using these.
  # # But you can also just use your own, e.g. '<nixpkgs>'
  # nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
  # # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # # and also the haskell.nix functionality itself as an overlay.
  # nixpkgsArgs = haskellNix.nixpkgsArgs;

# let
#   sources = import ./nix/sources.nix;
#   haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
#   nixpkgsSrc = import sources.nixpkgs {};

#   # Fetch the latest haskell.nix and import its default.nix
#   # haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
#   # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
#   # you will be more likely to get cache hits when using these.
#   # But you can also just use your own, e.g. '<nixpkgs>'
#   # nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
#   # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
#   # and also the haskell.nix functionality itself as an overlay.
#   nixpkgsArgs = haskellNix.nixpkgsArgs;
# in
# { pkgs ? import nixpkgsSrc nixpkgsArgs
# , haskellCompiler ? "ghc865"
# }:
# # 'cabalProject' generates a package set based on a cabal.project (and the corresponding .cabal files)
# pkgs.haskell-nix.cabalProject {
#   # 'cleanGit' cleans a source directory based on the files known by git
#   src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
#   ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
# }

  # Fetch the latest haskell.nix and import its default.nix
  # haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  # nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  # nixpkgsArgs = haskellNix.nixpkgsArgs;

let
  sources = import ./nix/sources.nix;
  haskellNix = import sources.haskell-nix {};
  pkgs = import haskellNix.sources.nixpkgs-1909 haskellNix.nixpkgsArgs;
in

pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { name = "CrossyToad"; src = ./.; };
  ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.ghc865;
}

# let
#   sources = import ./nix/sources.nix;
#   compilerVersion = "${ghc}";
#   pkgs = (import sources.iohk-nixpkgs) (import sources.iohk-hnix);
# in
# pkgs.haskell-nix.cabalProject {
#   src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
#   ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.\${compilerVersion};
# }
