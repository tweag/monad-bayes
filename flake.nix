{
  description = "A library for probabilistic programming in Haskell.";
  nixConfig = {
    extra-substituters = [
      "https://tweag-monad-bayes.cachix.org"
      "https://tweag-wasm.cachix.org"
    ];
    extra-trusted-public-keys = [
      "tweag-monad-bayes.cachix.org-1:tmmTZ+WvtUMpYWD4LAkfSuNKqSuJyL3N8ZVm/qYtqdc="
      "tweag-wasm.cachix.org-1:Eu5eBNIJvleiWMEzRBmH3/fzA6a604Umt4lZguKtAU4="
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
    haskell-nix-utils.url = "github:TerrorJack/haskell-nix-utils";
    jupyter-flake.url = "git+https://github.com/tweag/monad-bayes?ref=notebooks";
  };
  outputs = {
    self,
    nixpkgs,
    flake-compat,
    flake-utils,
    pre-commit-hooks,
    haskell-nix-utils,
    jupyter-flake,
  } @ inputs:
    flake-utils.lib.eachSystem
    [
      # Tier 1 - Tested in CI
      flake-utils.lib.system.x86_64-linux
      flake-utils.lib.system.x86_64-darwin
      # Tier 2 - Not tested in CI (at least for now)
      flake-utils.lib.system.aarch64-linux
      flake-utils.lib.system.aarch64-darwin
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
        monad-bayes = pkgs.haskell.packages.ghc902.callCabal2nixWithOptions "monad-bayes" src "--benchmark" {};
        cabal-docspec = let
          ce =
            haskell-nix-utils.packages.${system}.pkgs.callPackage
            (import "${haskell-nix-utils}/project/cabal-extras.nix") {
              self = haskell-nix-utils;
              inherit (haskell-nix-utils.packages.${system}) compiler-nix-name index-state;
            };
        in
          ce.cabal-docspec.components.exes.cabal-docspec;
        jupyterShell = inputs.jupyter-flake.devShell.${system};
        monad-bayes-dev = pkgs.mkShell {
          inputsFrom = [monad-bayes.env jupyterShell];
          packages = with pre-commit-hooks.packages.${system}; [
            alejandra
            cabal-fmt
            hlint
            ormolu
            cabal-docspec
          ];
          shellHook =
            pre-commit.shellHook
            + ''
              echo "=== monad-bayes development shell ==="
            '';
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          inherit src;
          hooks = {
            alejandra.enable = true;
            cabal-fmt.enable = true;
            hlint.enable = false;
            ormolu.enable = true;
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
