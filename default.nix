let
  hs-libbase64 = import (builtins.fetchTarball {
    url = "https://github.com/chessai/hs-libbase64-bindings/archive/e8a5194742f41ce4109b05098a2859e8052ad1c1.tar.gz";
    sha256 = "1xqfjqb1ghh8idnindc6gfr62d78m5cc6jpbhv1hja8lkdrl8qf8";
  }) {};
in
{
  compiler ? "ghc944",
  pkgs ? import <nixpkgs> {
    config = {
      allowBroken = false;
      allowUnfree = false;
    };

    overlays = [ ];
      /*(self: super: {
        # not in nixpkgs yet
        inherit (hs-libbase64) libbase64;

        "haskell.packages.${compiler}" = haskell.packages.${compiler}.override {
          overrides = hself: hsuper: {
            inherit (hs-libbase64) libbase64-bindings;
          };
        };
      })*/
  },
  returnShellEnv ? false,
}:

let
  nix-gitignore = import (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "9e80c4d83026fa6548bc53b1a6fab8549a6991f6";
    sha256 = "04n9chlpbifgc5pa3zx6ff3rji9am6msrbn1z3x1iinjz2xjfp4p";
  }) {};
in
pkgs.haskell.packages.${compiler}.developPackage {
  name = "base64";
  root = nix-gitignore.gitignoreSource ./.;

  overrides = self: super: {
    inherit (hs-libbase64) libbase64-bindings;
  };

  inherit returnShellEnv;
}
