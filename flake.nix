{
  description = "JupyterLab Flake";

  inputs = {
    jupyterWith.url = "github:tweag/jupyterWith";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    haskell-nix-utils.url = "github:TerrorJack/haskell-nix-utils";
    jupyter-flake.url = "git+https://github.com/tweag/monad-bayes?ref=notebooks";
  };

  inputs.nixpkgs.url = "nixpkgs/22.05";

  outputs = {
    self,
    nixpkgs,
    jupyterWith,
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
        myHaskellPackageOverlay = self: super: {
          myHaskellPackages = super.haskellPackages.override {
            overrides = hself: hsuper: rec {
              brick = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
                hself.callCabal2nixWithOptions "brick" (builtins.fetchGit {
                  url = "https://github.com/jtdaugherty/brick";
                  rev = "d196695ab7fee3e1b27e2b34ae7d96d369fe1723";
                }) "" {}
              ))) [];

              imatrix-sundials = super.haskell.lib.doJailbreak (
                super.haskell.lib.dontCheck (
                  hself.callCabal2nix "hmatrix-sundials" (builtins.fetchGit {
                    url = "https://github.com/novadiscovery/hmatrix-sundials";
                    rev = "76bfee5b5a8377dc3f7161514761946a60d4834a";
                  }) {
                    sundials_arkode = self.sundials;
                    sundials_cvode = self.sundials;
                    klu = self.suitesparse;
                    suitesparseconfig = self.suitesparse;
                    sundials_sunlinsolklu = self.sundials;
                    sundials_sunmatrixsparse = self.sundials;
                  }
                )
              );
            };
          };
        };

        pkgs = import nixpkgs {
          system = system;
          overlays = [myHaskellPackageOverlay] ++ nixpkgs.lib.attrValues jupyterWith.overlays;
          config.allowBroken = true;
        };

        inherit (nixpkgs) lib;

        # I don't know why we have this
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
        jupyterEnvironment = pkgs.jupyterlabWith {
          kernels = [iHaskell];
        };
      in rec {
        apps.jupterlab = {
          type = "app";
          program = "${jupyterEnvironment}/bin/jupyter-lab";
        };
        # Is this a spelling mistake?
        defaultApp = apps.jupterlab;
        devShell = jupyterEnvironment.env;
      }
    );
}
