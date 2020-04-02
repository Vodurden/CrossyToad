let
  sources = import ./nix/sources.nix;
  haskellNix = import sources.haskell-nix {};
  pkgs = import haskellNix.sources.nixpkgs-1909 {};
  hsPackages = import ./default.nix;
in
hsPackages.shellFor {
  withHoogle = true;

  buildInputs = with pkgs; [
    cabal-install
    flamegraph
    imagemagick7
    haskellPackages.ghc-prof-flamegraph
    haskellPackages.profiteur
  ];

  exactDeps = true;
}
