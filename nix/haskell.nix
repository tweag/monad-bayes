self: super:

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
  mkProject = stackYaml: args: self.haskell-nix.stackProject (args // {
    inherit stackYaml;
    src = self.lib.sourceByRegex ../. source;
  });

  cabal-install = ghc9.tool "cabal-install" "latest";
  # some tools need these overrides for ghc 9.0.2 so that exceptions and Cabal are reinstallable
  nonReinstallablePkgsModule =
    {
      nonReinstallablePkgs = [
        "rts"
        "ghc-heap"
        "ghc-prim"
        "integer-gmp"
        "integer-simple"
        "base"
        "deepseq"
        "array"
        "ghc-boot-th"
        "pretty"
        "template-haskell"
        # ghcjs custom packages
        "ghcjs-prim"
        "ghcjs-th"
        "ghc-bignum"
        "exceptions"
        "stm"
        "ghc-boot"
        "ghc"
        "Win32"
        "array"
        "binary"
        "bytestring"
        "containers"
        # "Cabal"
        "directory"
        "filepath"
        "ghc-boot"
        "ghc-compact"
        "ghc-prim"
        # "ghci" "haskeline"
        "hpc"
        "mtl"
        "parsec"
        "process"
        "text"
        "time"
        "transformers"
        "unix"
        "xhtml"
        "terminfo"
      ];
    };
  hlint = ghc9.tool "hlint" {
    version = "latest";
    modules = [ nonReinstallablePkgsModule ];
  };
  ormolu = ghc9.tool "ormolu" {
    version = "latest";
    modules = [
      ({ lib, ... }: {
        options.nonReinstallablePkgs =
          lib.mkOption { apply = lib.remove "Cabal"; };
      })
    ];
  };

  shellcheck = ghc9.tool "ShellCheck" "latest";

  refactor = ghc9.tool "apply-refact" {
    version = "latest";
    modules = [ nonReinstallablePkgsModule ];
  };

  ghc9 = mkProject "stack-ghc902.yaml" {
    shell = {
      tools = {
        ghcid = "latest";
        hindent = "latest";
        haskell-language-server = {
          version = "latest";
          modules = [ nonReinstallablePkgsModule ];
        };
      };
      buildInputs = [
        cabal-install
        hlint
        self.nixpkgs-fmt
        self.python37
        self.python37Packages.matplotlib
        self.python37Packages.pandas
        ormolu
        refactor
        shellcheck
        self.stack
        fix
        lint
      ];
    };
  };
  lint = self.callPackage ./lint.nix {
    inherit cabal-install hlint ormolu shellcheck;
  };
  fix = self.callPackage ./fix.nix {
    inherit cabal-install hlint ormolu refactor;
  };
in
{
  haskell-nix = self.lib.recursiveUpdate super.haskell-nix {
    packageToolName = {
      apply-refact = "refactor";
      ShellCheck = "shellcheck";
    };
  };

  monad-bayes = {
    inherit ghc9 fix lint;
    ghc88 = mkProject "stack-ghc884.yaml" { };
    ghc810 = mkProject "stack-ghc8107.yaml" { };
  };
}
