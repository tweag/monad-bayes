{
  name,
  availableKernels,
  extraArgs,
}: let
  pkgs = extraArgs.pkgs;
in
  availableKernels.haskell {
    inherit pkgs;
    name = "custom-${name}"; # must be unique
    displayName = "custom ${name}";
    extraHaskellPackages = p: [
      p.hvega
      p.lens
      p.log-domain
      p.katip
      p.ihaskell-hvega
      p.ihaskell-diagrams
      p.text
      p.diagrams
      p.diagrams-cairo
      p.aeson
      p.lens
      p.lens-aeson
      p.pretty-simple
      p.monad-loops
      p.hamilton
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
      extraArgs.monad-bayes
    ];
  }
