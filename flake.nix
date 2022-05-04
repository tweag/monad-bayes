{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        src = pkgs.lib.sourceByRegex self [
          "^benchmark.*$"
          "^models.*$"
          "^monad-bayes\.cabal$"
          "^src.*$"
          "^test.*$"
          "^.*\.md"
        ];
        monad-bayes = pkgs.haskell.packages.ghc922.callCabal2nixWithOptions "monad-bayes" src "--benchmark" {};
        devShell = pkgs.mkShell {
          inputsFrom = [ monad-bayes.env ];
        };
      in
      {
        defaultPackage = monad-bayes;
        packages.default = monad-bayes;

        checks = { inherit monad-bayes; };

        inherit devShell;
        devShells.default = devShell;
      }
    );
}
