let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

      backprop = super.haskell.lib.dontCheck (
        hself.callCabal2nix "backprop" (builtins.fetchGit {
          url = "file:////Users/dom/oplss-staton/backprop";
          rev = "96ba7d37a6f583a3e018b8dab50691bdfa482dc8";
        }) { });

      # backprop = hself.callCabal2nix "backprop" (builtins.fetchGit {
      #     url = "https://github.com/idontgetoutmuch/backprop";
      #     rev = "96ba7d37a6f583a3e018b8dab50691bdfa482dc8";
      #   }) { };

    };
  };
};

in

{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-22.05-darwin.tar.gz")
  {
    config.allowBroken = true;
    overlays = [ myHaskellPackageOverlay ];
  }
}:

let

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    ad backprop base bytestring cassava containers filepath
    histogram-fill log-domain math-functions mtl mwc-random
    optparse-applicative pipes pretty-simple random text time
    transformers vector
  ];

in

pkgs.stdenv.mkDerivation {
  name = "Whatever";

  buildInputs = [
    pkgs.libintlOrEmpty
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
  ];
}
