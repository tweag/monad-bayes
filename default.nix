{ pkgs ? (import ./nix/nixpkgs) }:

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
  mkMonadBayes = stackYaml: pkgs.haskell-nix.stackProject {
    inherit stackYaml;
    src = pkgs.lib.sourceByRegex ./. source;
  };
  monad-bayes-ghc84 = mkMonadBayes "stack-ghc844.yaml";
  monad-bayes-ghc86 = mkMonadBayes "stack-ghc865.yaml";
  monad-bayes-ghc88 = mkMonadBayes "stack-ghc881.yaml";
in {
  inherit monad-bayes-ghc84 monad-bayes-ghc86 monad-bayes-ghc88;
  shell = monad-bayes-ghc86.shellFor {
    packages = ps: [
      ps.monad-bayes
    ];
    buildInputs = with pkgs.haskell.packages.ghc865; [
      cabal-install
      ghcid
      hindent
      hlint
      pkgs.nixpkgs-fmt
      pkgs.python37
      pkgs.python37Packages.matplotlib
      pkgs.python37Packages.pandas
      pkgs.stack
    ];
  };
}
