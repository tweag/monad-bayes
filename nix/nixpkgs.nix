{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix { inherit system; }
}:

let
  pkgs = import inputs.nixpkgs { inherit system pkgs; };
  haskellNix = import inputs.haskellNix { inherit pkgs; };
in
import inputs.nixpkgs (haskellNix.nixpkgsArgs // { inherit system; })
