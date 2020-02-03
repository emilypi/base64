{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, bytestring, deepseq
      , gauge, memory, random-bytestring, stdenv, tasty, tasty-hunit
      , text
      }:
      mkDerivation {
        pname = "base64";
        version = "0.4.0";
        src = ./.;
        libraryHaskellDepends = [ base bytestring text ];
        testHaskellDepends = [
          base base64-bytestring bytestring random-bytestring tasty
          tasty-hunit text
        ];
        benchmarkHaskellDepends = [
          base base64-bytestring bytestring deepseq gauge memory
          random-bytestring text
        ];
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
