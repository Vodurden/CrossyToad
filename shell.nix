{ compiler ? "ghc844" }:

let
  pkgs = import ./nix/nixpkgs.nix {};
  crossyToad = import ./default.nix { inherit compiler; };
in
  if pkgs.lib.inNixShell then crossyToad.env else crossyToad
