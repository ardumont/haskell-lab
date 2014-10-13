{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

let
  inherit (haskellPackages)
    cabal
    cabalInstall_1_18_0_3
    HUnit
    parsec
    QuickCheck
    testFramework
    testFrameworkQuickcheck2
    text;
in cabal.mkDerivation (self: {
  pname = "haskell-lab";
  version = "0.0.0.0";
  sha256 = "dummy-sha";
  src = ./src;
  buildDepends = [
    HUnit parsec QuickCheck testFramework testFrameworkQuickcheck2 text
  ];
  buildTools = [
    cabalInstall_1_18_0_3
  ];
  meta = {
    homepage = "https://github.com/ardumont/haskell-lab.git";
    description = "Hacking Haskell";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
