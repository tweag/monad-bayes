{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-compat,
    flake-utils,
  }:
    flake-utils.lib.eachSystem
    [
      flake-utils.lib.system.x86_64-linux
      flake-utils.lib.system.aarch64-linux
    ]
    (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
