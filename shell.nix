{ compiler ? "ghc7101" }:

with (import <nixpkgs> {}).pkgs;
let ghc = haskell.packages.${compiler}.ghcWithPackages
            (pkgs: with pkgs; [ aeson lens monad-par HUnit parsec
                                QuickCheck
                                text digits mtl cabal-install hoogle ]);
in stdenv.mkDerivation {
  name = "my-haskell-lab-env";
  buildInputs = [ ghc ];
  shellHook = "eval $(grep export ${ghc}/bin/ghc)";
}
