let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
      random-fu = super.haskell.lib.dontCheck (hself.callCabal2nixWithOptions "random-fu" (builtins.fetchGit {
        url = "https://github.com/haskell-numerics/random-fu";
        rev = "b5cf96c6d904faa6adcf010a88f3ee117b289a4f";
      }) "--subpath random-fu" { });
      rvar = super.haskell.lib.dontCheck (hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
        url = "https://github.com/haskell-numerics/random-fu";
        rev = "b5cf96c6d904faa6adcf010a88f3ee117b289a4f";
      }) "--subpath rvar" { });
      monad-bayes = super.haskell.lib.dontCheck (
        super.haskell.lib.doJailbreak (
          super.haskell.lib.disableLibraryProfiling (hself.callPackage  ./. { })
        )
      );
   };
  };
};

in

{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-23.05-darwin.tar.gz")
  {
    overlays = [ myHaskellPackageOverlay ];
    config.allowBroken = true;
  }
}:

let

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    base criterion deepseq
    mersenne-random-pure64
    monad-bayes
    MonadRandom mtl mwc-random random random-fu stateref vector
  ];

in

pkgs.stdenv.mkDerivation {
  name = "simple-shell";

  buildInputs = [
    pkgs.cabal-install
    pkgs.cabal2nix
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ];
}
