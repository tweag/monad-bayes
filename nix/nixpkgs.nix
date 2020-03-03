let
  nixpkgsSrc = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsSrc.rev}.tar.gz";
    inherit (nixpkgsSrc) sha256;
  };
  haskellNixSrc = builtins.fromJSON (builtins.readFile ./haskell-nix.json);
  haskellNix = import (
    builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/${haskellNixSrc.rev}.tar.gz";
      inherit (haskellNixSrc) sha256;
    }
  );
  pkgs = import nixpkgs haskellNix;
in
pkgs
