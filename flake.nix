{
  description = "A library for probabilistic programming in Haskell.";
  nixConfig = {
    extra-substituters = [
      "https://tweag-monad-bayes.cachix.org"
    ];
    extra-trusted-public-keys = [
      "tweag-monad-bayes.cachix.org-1:tmmTZ+WvtUMpYWD4LAkfSuNKqSuJyL3N8ZVm/qYtqdc="
    ];
  };
  inputs = {
    # Not the indirect `nixpkgs/nixos-unstable`: that is resolved through the
    # flake registry of whoever runs `nix flake update`, which makes the shape
    # of the lock entry depend on the machine.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
  outputs =
    { self
    , nixpkgs
    , flake-compat
    , flake-utils
    , pre-commit-hooks
    ,
    } @ inputs:
    flake-utils.lib.eachSystem
      [
        # All of these are built in CI, see .github/workflows/nix.yml. The test
        # suite only runs on x86_64 though, see the `modifier` below.
        flake-utils.lib.system.x86_64-linux
        flake-utils.lib.system.aarch64-linux
        flake-utils.lib.system.aarch64-darwin
        # Note: no x86_64-darwin. nixpkgs 26.11 dropped support for it, and
        # importing nixpkgs for that system now throws rather than merely warns.
        # See https://github.com/NixOS/nixpkgs/pull/535508 and
        # https://nixos.org/manual/nixpkgs/unstable/release-notes#x86_64-darwin-26.11
        # If you still need it, the 26.05 branch supports it until end of 2026.
      ]
      (
        system:
        let
          inherit (nixpkgs) lib;
          pkgs = import nixpkgs {
            inherit system;
            config.allowBroken = true;
          };

          warnToUpdateNix = pkgs.lib.warn "Consider updating to Nix > 2.7 to remove this warning!";
          src = lib.sourceByRegex self [
            "^benchmark.*$"
            "^models.*$"
            "^monad-bayes\.cabal$"
            "^src.*$"
            "^test.*$"
            "^.*\.md"
          ];

          # Always keep this up to date with the tested-with section in monad-bayes.cabal!
          # and the build-all-ghcs job in .github/workflows/nix.yml!
          ghcs = [
            "ghc94"
            "ghc96"
            "ghc98"
            "ghc910"
            "ghc912"
            "default"
          ];

          allHaskellPackages = lib.filterAttrs (ghcVersion: _: builtins.elem ghcVersion ghcs) (pkgs.haskell.packages // { default = pkgs.haskellPackages; });

          # Please check after flake.lock updates whether some of these overrides can be removed
          haskellOverrides = self: super:
            with pkgs.haskell.lib;
            {
              # nixpkgs still ships brick 2.9, but we need >= 2.10
              brick = super.callHackageDirect {
                pkg = "brick";
                ver = "2.10";
                sha256 = "sha256-m1PvPySOuTZbcnCm4j7M7AihK0w8OGKumyRR3jU5nfw=";
              } { };
            }
            // lib.optionalAttrs (lib.versionAtLeast super.ghc.version "9.10") {
              microstache = doJailbreak super.microstache;
            };

          haskellPackagesFor = haskellPackages: haskellPackages.extend haskellOverrides;

          monad-bayes-for = haskellPackages: haskellPackages.developPackage {
            name = "monad-bayes";
            root = src;
            cabal2nixOptions = "--benchmark -fdev";

            # Only run the tests on x86_64, they fail on aarch64 because of machine
            # precision issues: the fixture tests compare `show`n `Double`s against
            # committed fixtures that were generated on x86_64, and IEEE 754 only
            # mandates correct rounding for the basic operations and `sqrt`, not for
            # `log`, `exp`, `log1p` or `**`. Those come from the platform's libm and
            # differ in the last ulp.
            # It is the architecture, not Apple's libm: five of the 45 examples fail
            # the same way on aarch64-linux against glibc, which is why we build
            # there in CI. See https://github.com/tweag/monad-bayes/pull/256,
            # https://github.com/tweag/monad-bayes/pull/389 and
            # https://github.com/tweag/monad-bayes/issues/368.
            modifier =
              if pkgs.stdenv.hostPlatform.isx86_64
              then lib.id
              else pkgs.haskell.lib.dontCheck;
            overrides = haskellOverrides;
          };

          monad-bayes-per-ghc = lib.mapAttrs (_: monad-bayes-for) allHaskellPackages;

          monad-bayes = monad-bayes-per-ghc.default;

          monad-bayes-all-ghcs = pkgs.linkFarm "monad-bayes-all-ghcs" monad-bayes-per-ghc;

          # A GHC that has IHaskell, monad-bayes and everything the notebooks need.
          ihaskellEnv = (haskellPackagesFor pkgs.haskellPackages).ghcWithPackages (p:
            [
              p.ihaskell
              p.ihaskell-blaze
              p.ihaskell-diagrams
            ]
            ++ (import ./kernels/haskell.nix { inherit monad-bayes; }) p);

          # Launcher for the kernel. This is what `ihaskell install` would write into
          # the kernelspec, except that we let GHC report its own paths instead of
          # hardcoding them.
          ihaskellKernel = pkgs.writeShellScript "monad-bayes-kernel" ''
            export GHC_PACKAGE_PATH="$(${ihaskellEnv}/bin/ghc --print-global-package-db)''${GHC_PACKAGE_PATH:+:$GHC_PACKAGE_PATH}"
            exec ${ihaskellEnv}/bin/ihaskell kernel "$1" \
              --ghclib "$(${ihaskellEnv}/bin/ghc --print-libdir)" \
              +RTS -M3g -N2 -RTS
          '';

          # A JupyterLab with an IHaskell kernel that has monad-bayes available.
          # Built straight from nixpkgs; we used to use tweag/jupyenv for this, but it
          # is unmaintained and pinned this flake to a years-old nixpkgs.
          jupyterEnvironment = pkgs.python3.buildEnv.override {
            extraLibs = with pkgs.python3.pkgs; [
              jupyterlab
              nbconvert
              notebook
            ];
            makeWrapperArgs = [
              "--prefix JUPYTER_PATH : ${pkgs.jupyter-kernel.create {
                definitions = pkgs.jupyter-kernel.default // {
                  monad-bayes = {
                    displayName = "monad-bayes";
                    language = "haskell";
                    argv = [ "${ihaskellKernel}" "{connection_file}" ];
                    logo32 = null;
                    logo64 = null;
                  };
                };
              }}"
            ];
          };


          pre-commit = pre-commit-hooks.lib.${system}.run {
            inherit src;
            hooks = {
              cabal-fmt.enable = true;
              hlint.enable = false;
              ormolu.enable = true;
            };
          };
          devShellFor = ghcVersion: haskellPackages: addJupyter: haskellPackages.shellFor {
            packages = hps: [
              (monad-bayes-for haskellPackages)
            ];
            nativeBuildInputs = with pre-commit-hooks.packages.${system}; [
              cabal-fmt
              hlint
              ormolu
            ]
            # Not haskellPackages.cabal-install: cabal is a tool, not a library
            # dependency, and on the older compilers the per-GHC one has to build
            # Cabal-syntax from source, which fails on GHC 9.4.
            ++ [ pkgs.cabal-install ]
            ++ lib.optional addJupyter jupyterEnvironment
            ++ (with haskellPackages; [
              haskell-language-server
            ]);
          };
        in
        rec {
          packages = {
            inherit monad-bayes monad-bayes-per-ghc monad-bayes-all-ghcs pre-commit jupyterEnvironment;
          };
          packages.default = packages.monad-bayes;
          checks = { inherit monad-bayes pre-commit; };
          devShells = lib.concatMapAttrs
            (ghcVersion: haskellPackages: {
              "${ghcVersion}" = devShellFor ghcVersion haskellPackages false;
              "${ghcVersion}-jupyter" = devShellFor ghcVersion haskellPackages true;
            })
            allHaskellPackages;
          # Needed for backwards compatibility with Nix versions <2.8
          defaultPackage = warnToUpdateNix packages.default;
          devShell = warnToUpdateNix devShells.default;
          formatter = pkgs.nixpkgs-fmt;
        }
      );
}
