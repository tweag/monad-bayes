#{pkgs ? import <nixpkgs> {}}: let
{pkgs}: let
  inherit (pkgs.haskellPackages) callCabal2nix;

  ihaskellSrc = builtins.fetchGit {
    url = https://github.com/IHaskell/IHaskell;
    rev = "7d0b9b070aa821db1a4d38826e146fd2c41d1c0b";
  };

  callDisplayPackage = name:
    callCabal2nix
    "ihaskell-${name}"
    "${ihaskellSrc}/ihaskell-display/ihaskell-${name}"
    {};
in {
  ihaskell-diagrams = callDisplayPackage "diagrams";
}
