{
  description = "A library for probabilistic programming in Haskell.";

  nixConfig = {
    extra-substituters = [
      "https://tweag-monad-bayes.cachix.org"
    ];
    extra-trusted-public-keys = [
      "tweag-monad-bayes.cachix.org-1:tmmTZ+WvtUMpYWD4LAkfSuNKqSuJyL3N8ZVm/qYtqdc="
    ];
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-compat,
    flake-utils,
    pre-commit-hooks,
  }:
    flake-utils.lib.eachSystem
    [
      flake-utils.lib.system.x86_64-linux
      flake-utils.lib.system.aarch64-linux
    ]
    (
      system: let
        inherit (nixpkgs) lib;

        pkgs = nixpkgs.legacyPackages.${system};

        warnToUpdateNix = pkgs.lib.warn "Consider updating to Nix > 2.7 to remove this warning!";

        src = lib.sourceByRegex self [
          "^benchmark.*$"
          "^models.*$"
          "^monad-bayes\.cabal$"
          "^src.*$"
          "^test.*$"
          "^.*\.md"
        ];

        monad-bayes = pkgs.haskell.packages.ghc922.callCabal2nixWithOptions "monad-bayes" src "--benchmark" {};

        monad-bayes-dev = pkgs.mkShell {
          inputsFrom = [monad-bayes.env];
          shellHook =
            pre-commit.shellHook
            + ''
              echo "=== monad-bayes development shell ==="
            '';
        };

        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = self;
          hooks = {
            alejandra.enable = true;
            hlint.enable = true;
          };
        };
      in rec {
        packages = {inherit monad-bayes pre-commit;};
        packages.default = packages.monad-bayes;

        checks = {inherit monad-bayes pre-commit;};

        devShells.default = monad-bayes-dev;

        # Needed for backwards compatibility with Nix versions <2.8
        defaultPackage = warnToUpdateNix packages.default;
        devShell = warnToUpdateNix devShells.default;
      }
    );
}
