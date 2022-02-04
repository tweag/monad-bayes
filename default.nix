{ system ? builtins.currentSystem
}:
let
  flake = builtins.getFlake (toString ./.);
  pkgs = flake.legacyPackages.${system};
  mkMonadBayes = project:
    let
      # https://github.com/input-output-hk/haskell.nix/issues/470 suggests a
      # better way to build benchmarks.
      benchmarks = project.monad-bayes.components.benchmarks;
    in
    pkgs.recurseIntoAttrs {
      "lib-and-test" = project.monad-bayes;
      "ssm-bench" = benchmarks.ssm-bench;
      "speed-bench" = benchmarks.speed-bench;
      "checks" = project.monad-bayes.checks;
    };

  monad-bayes-ghc88 = mkMonadBayes pkgs.monad-bayes.ghc88;
  monad-bayes-ghc810 = mkMonadBayes pkgs.monad-bayes.ghc810;
  monad-bayes-ghc9 = mkMonadBayes pkgs.monad-bayes.ghc9;
in
{
  inherit monad-bayes-ghc88 monad-bayes-ghc810 monad-bayes-ghc9;

  inherit (pkgs.monad-bayes) lint fix;
}
