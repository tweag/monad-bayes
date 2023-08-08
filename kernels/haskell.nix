{monad-bayes}:

{pkgs, ...}: {
  kernel.haskell.monad-bayes = {
    enable = true;
    name = "monad-bayes";
    displayName = "monad-bayes";
    extraHaskellPackages = p: [
      p.hvega
      p.lens
      p.log-domain
      p.katip
      p.ihaskell-hvega
      p.ihaskell-diagrams
      p.diagrams
      p.diagrams-cairo
      p.aeson
      p.lens
      p.lens-aeson
      p.pretty-simple
      p.monad-loops
      # hamilton seems unmaintained, unclear whether we can keep it. See https://github.com/tweag/monad-bayes/issues/300
      # p.hamilton
      p.hmatrix
      p.vector-sized
      p.linear
      p.recursion-schemes
      p.data-fix
      p.free
      p.comonad
      p.adjunctions
      p.distributive
      p.vector
      p.megaparsec
      p.histogram-fill
      # Strange build error which I can't fix:
      # > Configuring gloss-rendering-1.13.1.2...
      # >
      # > Setup: Encountered missing or private dependencies:
      # > bytestring >=0.11 && <0.12
      # p.gloss
      monad-bayes
    ];
  };
}
