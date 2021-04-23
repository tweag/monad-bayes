let
  haskellNixSrc = builtins.fromJSON (builtins.readFile ./haskell-nix.json);
  haskellNix = import
    (
      builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/${haskellNixSrc.rev}.tar.gz";
        inherit (haskellNixSrc) sha256;
      }
    )
    { };
in
import haskellNix.sources.nixpkgs-2009 haskellNix.nixpkgsArgs
