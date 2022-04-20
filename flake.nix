{
  description = "A very basic flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    haskellNix.url = "github:input-output-hk/haskell.nix";

    nixpkgs.follows = "haskellNix/nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, ... }:
    let
      inherit (haskellNix) config;
      overlays = [
        haskellNix.overlay
        (import ./nix/haskell.nix)
      ];
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system config overlays; };
        flakes = pkgs.lib.mapAttrs (_: p: p.flake { }) pkgs.monad-bayes;
        prefixPkgNames = prefix: pkgs.lib.mapAttrs' (n: pkgs.lib.nameValuePair "${prefix}:${n}");
        prefixed = ghc: attr: prefixPkgNames ghc flakes.${ghc}.${attr};
        combined = attr: prefixed "ghc88" attr // prefixed "ghc810" attr // prefixed "ghc9" attr;
      in
      {
        legacyPackages = pkgs;

        inherit (flakes.ghc9) devShell;

        devShells = builtins.mapAttrs (_: x: x.devShell) flakes // {
          default = flakes.ghc9.devShell;
        };

        packages = combined "packages";
        checks = combined "checks";
      }
    );
}
