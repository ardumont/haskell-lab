{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, digits, HUnit, mtl, parsec
      , process, QuickCheck, stdenv, test-framework
      , test-framework-quickcheck2, text
      }:
      mkDerivation {
        pname = "haskell-lab";
        version = "0.0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers digits HUnit mtl parsec process QuickCheck
          test-framework test-framework-quickcheck2 text
        ];
        homepage = "https://github.com/ardumont/haskell-lab.git";
        description = "Hacking Haskell";
        license = stdenv.lib.licenses.gpl2;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
