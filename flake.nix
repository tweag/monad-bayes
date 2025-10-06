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
      };
    };
    jupyenv = {
      url = "github:tweag/jupyenv";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
      };
    };
  };
  outputs =
    { self
    , nixpkgs
    , jupyenv
    , flake-compat
    , flake-utils
    , pre-commit-hooks
    ,
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
        system:
        let
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

          # Always keep this up to date with the tested-with section in monad-bayes.cabal!
          # and the build-all-ghcs job in .github/workflows/nix.yml!
          ghcs = [
            "ghc94"
            "ghc96"
            "ghc98"
            "ghc910"
            "default"
          ];

          allHaskellPackages = lib.filterAttrs (ghcVersion: _: builtins.elem ghcVersion ghcs) (pkgs.haskell.packages // { default = pkgs.haskellPackages; });

          monad-bayes-for = haskellPackages: haskellPackages.developPackage {
            name = "monad-bayes";
            root = src;
            cabal2nixOptions = "--benchmark -fdev";

            # https://github.com/tweag/monad-bayes/pull/256: Don't run tests on Mac because of machine precision issues
            modifier = drv:
              if system == "x86_64-linux"
              then drv
              else pkgs.haskell.lib.dontCheck drv;
            overrides = self: super:
              with pkgs.haskell.lib;
              {
                # Please check after flake.lock updates whether some of these overrides can be removed
                brick = super.callHackageDirect {
                  pkg = "brick";
                  ver = "2.10";
                  sha256 = "sha256-m1PvPySOuTZbcnCm4j7M7AihK0w8OGKumyRR3jU5nfw=";
                } { };
                vty = super.callHackageDirect {
                  pkg = "vty";
                  ver = "6.4";
                  sha256 = "sha256-xHtMfRaJVk95UTwh2QU8VL3MgXuLQOzTwqTa5oevZ5U=";
                } { };
              }
              // lib.optionalAttrs (lib.versionAtLeast super.ghc.version "9.10") {
                # Please check after flake.lock updates whether some of these overrides can be removed
                microstache = doJailbreak super.microstache;
              };
          };

          monad-bayes-per-ghc = lib.mapAttrs (_: monad-bayes-for) allHaskellPackages;

          monad-bayes = monad-bayes-per-ghc.default;

          monad-bayes-all-ghcs = pkgs.linkFarm "monad-bayes-all-ghcs" monad-bayes-per-ghc;

          jupyterEnvironment = mkJupyterlabNew {
            imports = [
              (import ./kernels/haskell.nix { inherit monad-bayes; })
            ];
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
          devShellFor = ghcVersion: haskellPackages: addJupyter: haskellPackages.shellFor {
            packages = hps: [
              (monad-bayes-for haskellPackages)
            ];
            nativeBuildInputs = with pre-commit-hooks.packages.${system}; [
              alejandra
              cabal-fmt
              hlint
              ormolu
            ] ++ lib.optional addJupyter jupyterEnvironment
            ++ (with haskellPackages; [
              haskell-language-server
            ]);
          };
        in
        rec {
          packages = {
            inherit monad-bayes monad-bayes-per-ghc monad-bayes-all-ghcs pre-commit jupyterEnvironment;
          };
          packages.default = packages.monad-bayes;
          checks = { inherit monad-bayes pre-commit; };
          devShells = lib.concatMapAttrs
            (ghcVersion: haskellPackages: {
              "${ghcVersion}" = devShellFor ghcVersion haskellPackages false;
              "${ghcVersion}-jupyter" = devShellFor ghcVersion haskellPackages true;
            })
            allHaskellPackages;
          # Needed for backwards compatibility with Nix versions <2.8
          defaultPackage = warnToUpdateNix packages.default;
          devShell = warnToUpdateNix devShells.default;
          formatter = pkgs.nixpkgs-fmt;
        }
      );
}
