{
  description = "A very basic flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        ghcs = [ "ghc884" "ghc8107" "ghc902" ];

        pkgs = import nixpkgs {
          inherit system;

          overlays = [
            (final: prev: {
              monad-bayes = pkgs.lib.genAttrs ghcs (ghc: (pkgs.haskell.packages.${ghc}.override {
                overrides = self: super: {
                  monad-bayes = pkgs.haskell.lib.doBenchmark (self.callPackage ./monad-bayes.nix { });
                  mwc-random = self.callPackage ./nix/mwc-random.nix { };
                };
              }).monad-bayes);
            })
          ];
        };
        packages = flake-utils.lib.flattenTree { monad-bayes = pkgs.recurseIntoAttrs pkgs.monad-bayes; };
        mkShell = drv: pkgs.mkShell {
          packages = [
            (pkgs.callPackage ./nix/lint.nix { })
            (pkgs.callPackage ./nix/fix.nix { inherit (pkgs.haskellPackages) apply-refact; })
          ];
          inputsFrom = [ drv.env ];
        };
      in
      {
        inherit packages;

        checks = packages;

        devShell = mkShell pkgs.monad-bayes.ghc902;

        devShells = builtins.mapAttrs (_: mkShell) pkgs.monad-bayes // {
          default = mkShell pkgs.monad-bayes.ghc902;
        };

        legacyPackages = pkgs;
      }
    );
}
