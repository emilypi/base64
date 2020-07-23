{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, bytestring
      , criterion, deepseq, ghc-byteorder, QuickCheck, random-bytestring
      , stdenv, tasty, tasty-hunit, tasty-quickcheck, text, text-short
      }:
      mkDerivation {
        pname = "base64";
        version = "0.4.2.2";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring deepseq ghc-byteorder text text-short
        ];
        testHaskellDepends = [
          base base64-bytestring bytestring QuickCheck random-bytestring
          tasty tasty-hunit tasty-quickcheck text text-short
        ];
        benchmarkHaskellDepends = [
          base base64-bytestring bytestring criterion deepseq
          random-bytestring text
        ];
        homepage = "https://github.com/emilypi/base64";
        description = "Fast RFC 4648-compliant Base64 encoding";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
