{ mkDerivation
, abstract-par
, base
, containers
, criterion
, free
, hspec
, ieee754
, lib
, log-domain
, math-functions
, monad-coroutine
, mtl
, mwc-random
, optparse-applicative
, process
, QuickCheck
, safe
, statistics
, time
, transformers
, vector
}:
mkDerivation {
  pname = "monad-bayes";
  version = "0.1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    containers
    free
    ieee754
    log-domain
    math-functions
    monad-coroutine
    mtl
    mwc-random
    safe
    statistics
    transformers
    vector
  ];
  executableHaskellDepends = [
    base
    containers
    log-domain
    mwc-random
    optparse-applicative
    time
    vector
  ];
  testHaskellDepends = [
    base
    hspec
    ieee754
    log-domain
    math-functions
    mtl
    QuickCheck
    transformers
    vector
  ];
  benchmarkHaskellDepends = [
    abstract-par
    base
    containers
    criterion
    log-domain
    mwc-random
    process
    vector
  ];
  homepage = "http://github.com/tweag/monad-bayes#readme";
  description = "A library for probabilistic programming";
  license = lib.licenses.mit;
}
