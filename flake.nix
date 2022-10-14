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
    # nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs.url = "nixpkgs/22.05";
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
        pkgs = import nixpkgs {
          system = system;
          overlays =
            # [myHaskellPackageOverlay] ++
            nixpkgs.lib.attrValues jupyterWith.overlays;
          config.allowBroken = true;
        };
        # pkgs = nixpkgs.legacyPackages.${system};
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

        iHaskell = pkgs.kernels.iHaskellWith {
          # Identifier that will appear on the Jupyter interface.
          name = "nixpkgs";
          # Libraries to be available to the kernel.
          packages = p:
            with p; [
              # pkgs.myHaskellPackages.imatrix-sundials
              pkgs.haskellPackages.hvega
              pkgs.haskellPackages.lens
              pkgs.haskellPackages.log-domain
              pkgs.haskellPackages.katip
              pkgs.haskellPackages.ihaskell-hvega
              pkgs.haskellPackages.ihaskell-diagrams
              pkgs.haskellPackages.text
              pkgs.haskellPackages.diagrams
              pkgs.haskellPackages.diagrams-cairo
              pkgs.haskellPackages.aeson
              pkgs.haskellPackages.lens
              pkgs.haskellPackages.lens-aeson
              pkgs.haskellPackages.pretty-simple
              pkgs.haskellPackages.monad-loops
              pkgs.haskellPackages.hamilton
              pkgs.haskellPackages.hmatrix
              pkgs.haskellPackages.vector-sized
              pkgs.haskellPackages.linear
              pkgs.haskellPackages.recursion-schemes
              pkgs.haskellPackages.data-fix
              pkgs.haskellPackages.free
              pkgs.haskellPackages.comonad
              pkgs.haskellPackages.adjunctions
              pkgs.haskellPackages.distributive
              pkgs.haskellPackages.vector
              pkgs.haskellPackages.megaparsec
              pkgs.haskellPackages.histogram-fill
              monad-bayes
            ];
          # Optional definition of `haskellPackages` to be used.
          # Useful for overlaying packages.
          haskellPackages = pkgs.haskell.packages.ghc902;
        };

        jupyterEnvironment = pkgs.jupyterlabWith {
          kernels = [iHaskell];
        };

        jupyterShell = rec {
          apps.jupterlab = {
            type = "app";
            program = "${jupyterEnvironment}/bin/jupyter-lab";
          };
          defaultApp = apps.jupyterlab;
          devShell = jupyterEnvironment.env;
        };

        # But what do we do with this?
        # jupyterShell = inputs.jupyter-flake.devShell.${system};

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
