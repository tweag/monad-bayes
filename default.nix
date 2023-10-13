{ mkDerivation, abstract-par, base, brick, containers, criterion
, directory, foldl, free, histogram-fill, hspec, ieee754
, integration, lens, lib, linear, log-domain, math-functions
, matrix, monad-coroutine, monad-extras, mtl, mwc-random
, optparse-applicative, pipes, pretty-simple, primitive, process
, QuickCheck, random, safe, scientific, statistics, text, time
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
    monad-extras mtl mwc-random pipes pretty-simple primitive random
    safe scientific statistics text vector vty
  ];
  executableHaskellDepends = [
    abstract-par base brick containers criterion directory foldl free
    histogram-fill hspec ieee754 integration lens linear log-domain
    math-functions matrix monad-coroutine monad-extras mtl mwc-random
    optparse-applicative pipes pretty-simple primitive process
    QuickCheck random safe scientific statistics text time transformers
    typed-process vector vty
  ];
  testHaskellDepends = [
    abstract-par base brick containers criterion directory foldl free
    histogram-fill hspec ieee754 integration lens linear log-domain
    math-functions matrix monad-coroutine monad-extras mtl mwc-random
    optparse-applicative pipes pretty-simple primitive process
    QuickCheck random safe scientific statistics text time transformers
    typed-process vector vty
  ];
  benchmarkHaskellDepends = [
    abstract-par base brick containers criterion directory foldl free
    histogram-fill hspec ieee754 integration lens linear log-domain
    math-functions matrix monad-coroutine monad-extras mtl mwc-random
    optparse-applicative pipes pretty-simple primitive process
    QuickCheck random safe scientific statistics text time transformers
    typed-process vector vty
  ];
  homepage = "http://github.com/tweag/monad-bayes#readme";
  description = "A library for probabilistic programming";
  license = lib.licenses.mit;
  mainProgram = "example";
}
