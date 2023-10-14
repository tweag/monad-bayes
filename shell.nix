{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, abstract-par, base, brick, containers
      , criterion, directory, foldl, free, histogram-fill, hspec, ieee754
      , integration, lens, lib, linear, log-domain, math-functions
      , matrix, monad-coroutine, monad-extras, mtl, mwc-random
      , optparse-applicative, pipes, pretty-simple, primitive, process
      , QuickCheck, random, random-fu, safe, scientific, statistics, text, time
      , transformers, typed-process, vector, vty
      }:
      mkDerivation {
        pname = "monad-bayes";
        version = "1.2.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base brick containers foldl free histogram-fill ieee754 integration
          lens linear log-domain math-functions matrix monad-coroutine
          monad-extras mtl mwc-random pipes pretty-simple primitive random random-fu
          safe scientific statistics text vector vty
        ];
        executableHaskellDepends = [
          abstract-par base brick containers criterion directory foldl free
          histogram-fill hspec ieee754 integration lens linear log-domain
          math-functions matrix monad-coroutine monad-extras mtl mwc-random
          optparse-applicative pipes pretty-simple primitive process
          QuickCheck random random-fu safe scientific statistics text time transformers
          typed-process vector vty
        ];
        testHaskellDepends = [
          abstract-par base brick containers criterion directory foldl free
          histogram-fill hspec ieee754 integration lens linear log-domain
          math-functions matrix monad-coroutine monad-extras mtl mwc-random
          optparse-applicative pipes pretty-simple primitive process
          QuickCheck random random-fu safe scientific statistics text time transformers
          typed-process vector vty
        ];
        benchmarkHaskellDepends = [
          abstract-par base brick containers criterion directory foldl free
          histogram-fill hspec ieee754 integration lens linear log-domain
          math-functions matrix monad-coroutine monad-extras mtl mwc-random
          optparse-applicative pipes pretty-simple primitive process
          QuickCheck random random-fu safe scientific statistics text time transformers
          typed-process vector vty
        ];
        homepage = "http://github.com/tweag/monad-bayes#readme";
        description = "A library for probabilistic programming";
        license = lib.licenses.mit;
        mainProgram = "example";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
