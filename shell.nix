let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
    };
  };
};

in

{ nixpkgs ? import <nixpkgs> { overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:


let

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    abstract-par base brick containers criterion directory foldl free
    histogram-fill hspec ieee754 integration lens linear log-domain
    math-functions matrix monad-coroutine monad-extras mtl mwc-random
    optparse-applicative pipes pretty-simple primitive process
    QuickCheck random random-fu safe scientific statistics text time transformers
    typed-process vector vty
  ];

in

pkgs.stdenv.mkDerivation {
  name = "whatever";

  buildInputs = [
    pkgs.libintlOrEmpty
    pkgs.cabal-install
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ];
}
