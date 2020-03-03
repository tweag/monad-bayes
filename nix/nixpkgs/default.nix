let
  rev = "e2b4abe3c8f2e09adfc6a52007841d1b96c89371";
  sha256 = "sha256:1l3jfr74s7wmx3fk5y76ayazbfclvnr532kg1hypbzydp3n372rz";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  haskellNixRev = "fee0922de5e43bd1dc0e8f769e765894242c00a0";
  haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/${haskellNixRev}.tar.gz");
  pkgs = import nixpkgs haskellNix;
in
pkgs
