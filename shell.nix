{ pkgs ? (import ./nix/nixpkgs.nix) }:

(import ./default.nix { inherit pkgs; }).shell
