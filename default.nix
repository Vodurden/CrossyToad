{ # Nix dependencies
  mkDerivation, stdenv, darwin, hpack, cabal-install

  # Haskell lib dependencies
  , base, sdl2, sdl2-ttf, sdl2-image, linear, mtl, transformers, containers, lens, text, zippers

  # Haskell test dependencies
  , tasty, tasty-discover, tasty-hspec, hspec, tasty-hedgehog, hedgehog

  # Extra executables
  , flamegraph, ghc-prof-flamegraph
}:
mkDerivation {
  pname = "crossy-toad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    flamegraph ghc-prof-flamegraph cabal-install hpack
  ] ++ (if stdenv.isDarwin then [darwin.apple_sdk.frameworks.OpenGL] else []);
  executableHaskellDepends = [
    base sdl2 sdl2-ttf sdl2-image linear mtl transformers containers lens text zippers
    tasty tasty-discover tasty-hspec hspec tasty-hedgehog hedgehog
  ];
  license = stdenv.lib.licenses.bsd3;
}
