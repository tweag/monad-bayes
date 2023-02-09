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
    jupyterWith.url = "github:tweag/jupyterWith";
  };
  outputs = {
    self,
    nixpkgs,
    jupyterWith,
    flake-compat,
    flake-utils,
    pre-commit-hooks,
    haskell-nix-utils,
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
        inherit (jupyterWith.lib.${system}) mkJupyterlabFromPath;
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        warnToUpdateNix = pkgs.lib.warn "Consider updating to Nix > 2.7 to remove this warning!";
        src = lib.sourceByRegex self [
          "^benchmark.*$"
          "^models.*$"
          "^monad-bayes\.cabal$"
          "^src.*$"
          "^test.*$"
          "^.*\.md"
        ];

        monad-bayes = pkgs.haskell.packages.ghc902.developPackage {
          name = "monad-bayes";
          root = src;

          # Remove this override when bumping nixpkgs
          source-overrides = {
            vty = pkgs.fetchzip {
              url = "mirror://hackage/vty-5.37/vty-5.37.tar.gz";
              sha256 = "sha256-OOrJBi/mSIyaibgObrp6NmUTWxRu9pxmjAL0EuPV9wY=";
            };

            text-zipper = pkgs.fetchzip {
              url = "mirror://hackage/text-zipper-0.12/text-zipper-0.12.tar.gz";
              sha256 = "sha256-P2/UHuG3UuSN7G31DyYvyUWSyIj2YXAOmjGkHtTaP8o=";
            };

            bimap = pkgs.fetchzip {
              url = "mirror://hackage/bimap-0.5.0/bimap-0.5.0.tar.gz";
              sha256 = "sha256-pbw+xg9Qz/c7YoXAJg8SR11RJGmgMw5hhnzKv+bGK9w=";
            };

            brick = pkgs.fetchzip {
              url = "mirror://hackage/brick-1.4/brick-1.4.tar.gz";
              sha256 = "sha256-KDa7RVQQPpinkJ0aKsYP0E50pn2auEIP38l6Uk7GmmE=";
            };
          };

          cabal2nixOptions = "--benchmark";
        };

        jupyterEnvironment = mkJupyterlabFromPath ./kernels {inherit pkgs monad-bayes;};

        cabal-docspec = let
          ce =
            haskell-nix-utils.packages.${system}.pkgs.callPackage
            (import "${haskell-nix-utils}/project/cabal-extras.nix") {
              self = haskell-nix-utils;
              inherit (haskell-nix-utils.packages.${system}) compiler-nix-name index-state;
            };
        in
          ce.cabal-docspec.components.exes.cabal-docspec;

        monad-bayes-dev = pkgs.mkShell {
          inputsFrom = [monad-bayes.env];
          packages = with pre-commit-hooks.packages.${system}; [
            alejandra
            cabal-fmt
            hlint
            ormolu
            cabal-docspec
            jupyterEnvironment
          ];
          shellHook = pre-commit.shellHook;
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
        packages = {inherit monad-bayes pre-commit jupyterEnvironment;};
        packages.default = packages.monad-bayes;
        checks = {inherit monad-bayes pre-commit;};
        devShells.default = monad-bayes-dev;
        # Needed for backwards compatibility with Nix versions <2.8
        defaultPackage = warnToUpdateNix packages.default;
        devShell = warnToUpdateNix devShells.default;
      }
    );
}
