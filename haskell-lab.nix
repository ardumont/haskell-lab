{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

let
  inherit (haskellPackages)
    cabal
    cabalInstall
    HUnit
    parsec
    QuickCheck
    testFramework
    testFrameworkQuickcheck2
    text
    digits;
in cabal.mkDerivation (self: {
  pname = "haskell-lab";
  version = "0.0.0.0";
  sha256 = "dummy-sha";
  src = ./src;
  buildDepends = [
    HUnit parsec QuickCheck testFramework testFrameworkQuickcheck2 text digits
  ];
  buildTools = [
    cabalInstall
  ];
  meta = {
    homepage = "https://github.com/ardumont/haskell-lab.git";
    description = "Hacking Haskell";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
