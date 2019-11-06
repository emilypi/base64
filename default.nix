{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, deepseq, QuickCheck, stdenv
      , tasty, tasty-hunit, text
      }:
      mkDerivation {
        pname = "base64";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base bytestring deepseq text ];
        testHaskellDepends = [ base QuickCheck tasty tasty-hunit ];
        homepage = "https://github.com/emilypi/base64";
        description = "RFC 4648-compliant padded and unpadded base64 and base64url encodings";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
