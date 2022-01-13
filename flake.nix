{
  description = "A very basic flake";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    haskellNix.url = "github:input-output-hk/haskell.nix";

    nixpkgs.follows = "haskellNix/nixpkgs";
  };

  outputs = inputs@{ self, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import ./nix/nixpkgs.nix { inherit system inputs; };
        projects = import ./nix/projects.nix { inherit system pkgs; };
        flakes = pkgs.lib.mapAttrs (_: p: p.flake { }) projects;
        prefixPkgNames = prefix: pkgs.lib.mapAttrs' (n: pkgs.lib.nameValuePair "${prefix}:${n}");
        prefixed = ghc: attr: prefixPkgNames ghc flakes."monad-bayes-${ghc}".${attr};
        combined = attr: prefixed "ghc88" attr // prefixed "ghc810" attr // prefixed "ghc9" attr;
      in
      {
        inherit (flakes.monad-bayes-ghc9) devShell;
        packages = combined "packages";
        checks = combined "checks";
      }
    );
}
