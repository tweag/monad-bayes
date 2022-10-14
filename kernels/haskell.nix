{
  pkgs,
  availableKernels,
  kernelName,
}:
availableKernels.haskell.override {
  name = "custom-${kernelName}"; # must be unique
  displayName = "custom ${kernelName}";
  extraHaskellPackages = p:
    with p; [
      pkgs.haskellPackages.hvega
      pkgs.haskellPackages.lens
      pkgs.haskellPackages.log-domain
      pkgs.haskellPackages.katip
      pkgs.haskellPackages.ihaskell-hvega
      pkgs.haskellPackages.ihaskell-diagrams
      pkgs.haskellPackages.text
      pkgs.haskellPackages.diagrams
      pkgs.haskellPackages.diagrams-cairo
      pkgs.haskellPackages.aeson
      pkgs.haskellPackages.lens
      pkgs.haskellPackages.lens-aeson
      pkgs.haskellPackages.pretty-simple
      pkgs.haskellPackages.monad-loops
      pkgs.haskellPackages.hamilton
      pkgs.haskellPackages.hmatrix
      pkgs.haskellPackages.vector-sized
      pkgs.haskellPackages.linear
      pkgs.haskellPackages.recursion-schemes
      pkgs.haskellPackages.data-fix
      pkgs.haskellPackages.free
      pkgs.haskellPackages.comonad
      pkgs.haskellPackages.adjunctions
      pkgs.haskellPackages.distributive
      pkgs.haskellPackages.vector
      pkgs.haskellPackages.megaparsec
      pkgs.haskellPackages.histogram-fill
      monad-bayes
    ];
}
