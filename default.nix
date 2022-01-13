{ system ? builtins.currentSystem
, pkgs ? import ./nix/nixpkgs.nix { inherit system; }
}:
let
  mkMonadBayes = project:
    let
      # https://github.com/input-output-hk/haskell.nix/issues/470 suggests a
      # better way to build benchmarks.
      benchmarks = project.monad-bayes.components.benchmarks;
    in
    pkgs.recurseIntoAttrs {
      "shellFor" = project.shellFor;
      "lib-and-test" = project.monad-bayes;
      "ssm-bench" = benchmarks.ssm-bench;
      "speed-bench" = benchmarks.speed-bench;
      "checks" = project.monad-bayes.checks;
    };

  projects = import ./nix/projects.nix { inherit pkgs; };
  monad-bayes-ghc88 = mkMonadBayes projects.monad-bayes-ghc88;
  monad-bayes-ghc810 = mkMonadBayes projects.monad-bayes-ghc810;
  monad-bayes-ghc9 = mkMonadBayes projects.monad-bayes-ghc9;

  defaultProject = projects.monad-bayes-ghc810;
  defaultHsPkgs = defaultProject.hsPkgs;


  cabal-install = defaultProject.tool "cabal-install" "latest";
  hlint = defaultProject.tool "hlint" "latest";
  ormolu = defaultProject.tool "ormolu" {
    version = "latest";
    modules = [
      ({ lib, ... }: {
        options.nonReinstallablePkgs =
          lib.mkOption { apply = lib.remove "Cabal"; };
      })
    ];
  };

  inherit (defaultHsPkgs.ShellCheck.components.exes) shellcheck;

  inherit (defaultHsPkgs.apply-refact.components.exes) refactor;
in
{
  inherit monad-bayes-ghc88 monad-bayes-ghc810 monad-bayes-ghc9;

  shell = defaultProject.shellFor {
    tools = {
      ghcid = "latest";
      hindent = "latest";
      haskell-language-server = "latest";
    };
    buildInputs = [
      cabal-install
      hlint
      pkgs.nixpkgs-fmt
      pkgs.python37
      pkgs.python37Packages.matplotlib
      pkgs.python37Packages.pandas
      ormolu
      refactor
      shellcheck
      pkgs.stack
    ];
  };
  lint = pkgs.callPackage ./nix/lint.nix {
    inherit cabal-install hlint ormolu shellcheck;
  };
  fix = pkgs.callPackage ./nix/fix.nix {
    inherit cabal-install hlint ormolu refactor;
  };
}
