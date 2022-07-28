{
  description = "JupyterLab Flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    haskell-nix-utils.url = "github:TerrorJack/haskell-nix-utils";
  };

  inputs.nixpkgs.url = "nixpkgs/22.05";

  outputs = {
    self,
    nixpkgs,
    flake-compat,
    flake-utils,
    pre-commit-hooks,
    haskell-nix-utils,
  }:
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

        monad-bayes = pkgs.myHaskellPackages.callCabal2nixWithOptions "monad-bayes" src "--benchmark" {};

        iHaskell = pkgs.kernels.iHaskellWith {
          # Identifier that will appear on the Jupyter interface.
          name = "nixpkgs";
          # Libraries to be available to the kernel.
          packages = p:
            with p; [
              pkgs.myHaskellPackages.imatrix-sundials
              pkgs.myHaskellPackages.hvega
              pkgs.myHaskellPackages.lens
              pkgs.myHaskellPackages.lens-aeson
              pkgs.myHaskellPackages.aeson
              pkgs.myHaskellPackages.megaparsec
              pkgs.myHaskellPackages.hamilton
              pkgs.myHaskellPackages.log-domain
              pkgs.myHaskellPackages.katip
              pkgs.myHaskellPackages.ihaskell-hvega
              pkgs.myHaskellPackages.ihaskell-diagrams
              pkgs.myHaskellPackages.text
              pkgs.myHaskellPackages.diagrams
              pkgs.myHaskellPackages.diagrams-cairo
              monad-bayes
            ];
          # Optional definition of `haskellPackages` to be used.
          # Useful for overlaying packages.
          haskellPackages = pkgs.haskell.packages.ghc902;
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
