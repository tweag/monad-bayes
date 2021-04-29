{ pkgs ? (import ./nix/nixpkgs.nix) }:
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
  mkMonadBayes = stackYaml: doBenchAndTest:
    let
      project = pkgs.haskell-nix.stackProject {
        inherit stackYaml;
        src = pkgs.lib.sourceByRegex ./. source;
      };
      # https://github.com/input-output-hk/haskell.nix/issues/470 suggests a
      # better way to build benchmarks.
      benchmarks = project.monad-bayes.components.benchmarks;
    in
    pkgs.recurseIntoAttrs
      {
        "shellFor" = project.shellFor;
        "lib-and-test" = project.monad-bayes;
      } // (
      if doBenchAndTest
      then {
        "ssm-bench" = benchmarks.ssm-bench;
        "speed-bench" = benchmarks.speed-bench;
        "checks" = project.monad-bayes.checks;
      }
      else { }
    );
  monad-bayes-ghc84 = mkMonadBayes "stack-ghc844.yaml" false;
  monad-bayes-ghc86 = mkMonadBayes "stack-ghc865.yaml" true;
  monad-bayes-ghc88 = mkMonadBayes "stack-ghc883.yaml" true;
  monad-bayes-ghc810 = mkMonadBayes "stack-ghc8104.yaml" true;

  defaultHaskellPackages = pkgs.haskell.packages.ghc865;
in
{
  inherit monad-bayes-ghc84 monad-bayes-ghc86 monad-bayes-ghc88;
  shell = monad-bayes-ghc86.shellFor {
    packages = ps: [
      ps.monad-bayes
    ];
    buildInputs = with defaultHaskellPackages; [
      apply-refact
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
  lint = defaultHaskellPackages.callPackage ./nix/lint.nix { };
  fix = defaultHaskellPackages.callPackage ./nix/fix.nix { };
  update = defaultHaskellPackages.callPackage ./nix/update.nix { };
}
