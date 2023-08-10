{
  description = "A library for probabilistic programming in Haskell.";
  nixConfig = {
    extra-substituters = [
      "https://tweag-monad-bayes.cachix.org"
      "https://tweag-jupyter.cachix.org"
    ];
    extra-trusted-public-keys = [
      "tweag-monad-bayes.cachix.org-1:tmmTZ+WvtUMpYWD4LAkfSuNKqSuJyL3N8ZVm/qYtqdc="
      "tweag-jupyter.cachix.org-1:UtNH4Zs6hVUFpFBTLaA4ejYavPo5EFFqgd7G7FxGW9g="
    ];
  };
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    jupyenv = {
      url = "github:tweag/jupyenv";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
       };
    };
  };
  outputs = {
    self,
    nixpkgs,
    jupyenv,
    flake-compat,
    flake-utils,
    pre-commit-hooks,
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
        inherit (jupyenv.lib.${system}) mkJupyterlabNew;
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

        monad-bayes-per-ghc = let
          opts = {
            name = "monad-bayes";
            root = src;
            cabal2nixOptions = "--benchmark -fdev";

            # https://github.com/tweag/monad-bayes/pull/256: Don't run tests on Mac because of machine precision issues
            modifier = drv: if system == "x86_64-linux" then drv else pkgs.haskell.lib.dontCheck drv;
            overrides = self: super: with pkgs.haskell.lib; { # Please check after flake.lock updates whether some of these overrides can be removed
              string-qq = dontCheck super.string-qq;
              hspec = super.hspec_2_11_1;
              lens = super.lens_5_2_2;
              linear = super.linear_1_22;
              vty = super.vty_5_38;
            };
          };
          ghcs = [ # Always keep this up to date with the tested-with section in monad-bayes.cabal!
            "ghc902"
            "ghc927"
            "ghc945"
          ];
          buildForVersion = ghcVersion: (builtins.getAttr ghcVersion pkgs.haskell.packages).developPackage opts;
          in lib.attrsets.genAttrs ghcs buildForVersion;

        monad-bayes = monad-bayes-per-ghc.ghc902;

        monad-bayes-all-ghcs = pkgs.linkFarm "monad-bayes-all-ghcs" monad-bayes-per-ghc;

        jupyterEnvironment = mkJupyterlabNew {
          imports = [
            (import ./kernels/haskell.nix {inherit monad-bayes;})
          ];
        };

        monad-bayes-dev = pkgs.mkShell {
          inputsFrom = [monad-bayes.env];
          packages = with pre-commit-hooks.packages.${system}; [
            alejandra
            cabal-fmt
            hlint
            ormolu
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
        packages = {
          inherit monad-bayes monad-bayes-per-ghc monad-bayes-all-ghcs pre-commit jupyterEnvironment;
        };
        packages.default = packages.monad-bayes;
        checks = {inherit monad-bayes pre-commit;};
        devShells.default = monad-bayes-dev;
        # Needed for backwards compatibility with Nix versions <2.8
        defaultPackage = warnToUpdateNix packages.default;
        devShell = warnToUpdateNix devShells.default;
      }
    );
}
