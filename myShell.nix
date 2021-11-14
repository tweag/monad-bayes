let

myHaskellPackageOverlay = self: super: {

  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

      random-fu = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "random-fu" (builtins.fetchGit {
          url = "https://github.com/lehins/random-fu";
          rev = "1eb5c0080618a691cd9fa81ac3d0ea29edeb084f";
          ref = "switch-to-random";
        }) "--subpath random-fu" { }
      ))) [ ];

      rvar = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
          url = "https://github.com/lehins/random-fu";
          rev = "1eb5c0080618a691cd9fa81ac3d0ea29edeb084f";
          ref = "switch-to-random";
        }) "--subpath rvar" { }
      ))) [ ];

      random = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
          url = "https://github.com/haskell/random";
          rev = "edae4f7908f3c7e00be1094034a4a09cd72ab35e";
        }) "" { }
      ))) [ ];

      hashable = super.haskell.lib.doJailbreak hsuper.hashable;

      mwc-random = hself.callHackage "mwc-random" "0.15.0.1" {};

      random-fu-multivariate =  hsuper.random-fu-multivariate;

    };
  };
};

in

{ nixpkgs ? import <nixpkgs> { config.allowBroken = true; overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:

 let

  haskellDeps = ps: with ps; [
    base criterion derive-storable free generic-deriving
    hmatrix log-domain mtl monad-coroutine mwc-random
    random random-fu random-fu-multivariate
    statistics vector
    # (nixpkgs.haskell.lib.doJailbreak monad-bayes)
  ];

in

  nixpkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = with nixpkgs.rPackages; [
    nixpkgs.cabal-install
    (nixpkgs.myHaskellPackages.ghcWithPackages haskellDeps)
  ];
  }
