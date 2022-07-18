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
    jupyterWith.url = "github:tweag/jupyterWith";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    jupyterWith,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
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
              pkgs.myHaskellPackages.log-domain
              pkgs.myHaskellPackages.katip
              pkgs.myHaskellPackages.ihaskell-hvega
              pkgs.myHaskellPackages.ihaskell-diagrams
              pkgs.myHaskellPackages.text
              pkgs.myHaskellPackages.diagrams
              pkgs.myHaskellPackages.diagrams-cairo
              pkgs.myHaskellPackages.aeson
              pkgs.myHaskellPackages.lens
              pkgs.myHaskellPackages.lens-aeson
              pkgs.myHaskellPackages.pretty-simple
              pkgs.myHaskellPackages.monad-loops
              pkgs.myHaskellPackages.hamilton
              pkgs.myHaskellPackages.hmatrix
              pkgs.myHaskellPackages.vector-sized
              pkgs.myHaskellPackages.linear
              pkgs.myHaskellPackages.recursion-schemes
              pkgs.myHaskellPackages.data-fix
              pkgs.myHaskellPackages.free
              pkgs.myHaskellPackages.comonad
              pkgs.myHaskellPackages.adjunctions
              pkgs.myHaskellPackages.distributive
              pkgs.myHaskellPackages.vector
              pkgs.myHaskellPackages.megaparsec
              monad-bayes
            ];
          # Optional definition of `haskellPackages` to be used.
          # Useful for overlaying packages.
          haskellPackages = pkgs.haskell.packages.ghc902;
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

