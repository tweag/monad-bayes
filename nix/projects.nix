{ system ? builtins.currentSystem
, pkgs ? import ./nixpkgs.nix { inherit system; }
}:

let
  source = [
    "^.*\.md"
    "^benchmark.*$"
    "^examples.*$"
    "^models.*$"
    "^monad-bayes\.cabal$"
    "^src.*$"
    "^stack.*\.yaml$"
    "^test.*$"
  ];
  mkProject = stackYaml: pkgs.haskell-nix.stackProject {
    inherit stackYaml;
    src = pkgs.lib.sourceByRegex ./.. source;
  };
in {
  monad-bayes-ghc88 = mkProject "stack-ghc884.yaml";
  monad-bayes-ghc810 = mkProject "stack-ghc8107.yaml";
  monad-bayes-ghc9 = mkProject "stack-ghc902.yaml";
}
