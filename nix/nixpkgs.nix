let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-version.json);
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/${pinnedVersion.rev}.tar.gz";
    sha256 = pinnedVersion.sha256;
  };
in
  import nixpkgs
