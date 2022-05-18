let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  nixpkgs = import pkgs.nixpkgs {};
  compiler = "ghc8107";
  packages = self: with self; [ megaparsec hvega ihaskell-hvega ihaskell-aeson ihaskell-blaze ihaskell-charts ihaskell-diagrams ];
}